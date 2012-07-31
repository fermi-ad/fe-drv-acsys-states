%%% $Id: states_dev.erl,v 1.6 2012/06/15 20:32:50 neswold Exp $

%%% States Front-end Driver
%%%
%%% This device driver implements a stripped down STATES facility
%%% similar to the one in the real control system. This driver is
%%% simpler in that it only supports multicasting state changes due to
%%% a setting. If necessary, we can add the other complexity.

-module(states_dev).

-include_lib("acnet/include/acnet.hrl").
-include_lib("daq/include/devices.hrl").
-include_lib("daq/include/setdat_protocol.hrl").

-export([start/1]).
-export([init_task/0, init_fsmset/1]).

-import(error_logger, [info_msg/1, info_msg/2, warning_msg/2]).

-define(ATOMIC_SIZE, 2).

-record(mystate, {table=ets:new(stateDevTable, [set, private]),
		  di_alive=undefined :: undefined | integer(),
		  last_alive=os:timestamp(),
		  count_alive=0,
		  socket,
		  seq=0}).

update_status(Tid, DI, State) ->
    case ets:lookup(Tid, DI) of
	[] -> ok;
	[{_, Val, _}] ->
	    ets:insert(Tid, {DI, Val, State})
    end.

update_value(Tid, DI, Val) ->
    case ets:lookup(Tid, DI) of
	[] ->
	    ets:insert(Tid, {DI, Val, true}),
	    false;

	[{_, _, State}] ->
	    ets:insert(Tid, {DI, Val, State}),
	    State
    end.

%%% Multicast a STATES protocol message.

report_new_state(#mystate{table=Tid, seq=Seq} = S, DI, Val) ->
    {MSec, Sec, USec} = os:timestamp(),
    case update_value(Tid, DI, Val) of
	true ->
	    Data = [<<1:16/little, Seq:16/little, 1:32/little>>,
		    <<0:16/little, DI:32/little, Val:16/little>>,
		    <<(MSec * 1000000 + Sec):32/little,
		      (USec * 1000):32/little>>],
	    acnet:send_usm(fsmset, "STATES@STATES", Data),
	    S#mystate{seq=(Seq + 1) band 16#ffff};

	false ->
	    S
    end.

%%% Calculate the delay needed to wait until 5 seconds after the last
%%% timeout. If the system doesn't have a DI defined, then we wait
%%% forever.

calc_delay(#mystate{di_alive=undefined}) ->
    infinity;
calc_delay(#mystate{last_alive=Last}) ->
    Delta = timer:now_diff(os:timestamp(), Last) div 1000,
    max(5000 - Delta, 0).

%%% Add five seconds to the provided timeout. The microsecond field is
%%% always cleared to 0.

add_5_sec({MS, S, _}) ->
    NS = S + 5,
    {MS + (NS div 1000000), NS rem 1000000, 0}.

%%% Handle requests to the table.

loop(#mystate{table=Tid, last_alive=Last, count_alive=Count} = S) ->
    NS = receive
	     { CPid, set, DI, Val, Min, Max } ->
		 if
		     Val =< Max andalso Val >= Min ->
			 CPid ! ok,
			 report_new_state(S, DI, Val);

		     true ->
			 CPid ! {error, illegal_val},
			 S
		 end;

	     { CPid, read, DI } ->
		 case ets:lookup(Tid, DI) of
		     [] -> CPid ! {error, not_found};
		     [{_, Val, _}] -> CPid ! {ok, Val}
		 end,
		 S;

	     { CPid, enable, State, DI } ->
		 CPid ! ok,
		 update_status(Tid, DI, State),
		 S;

	     { CPid, status, DI } ->
		 case ets:lookup(Tid, DI) of
		     [] -> CPid ! {ok, false};
		     [{_, _, Val}] -> CPid ! {ok, Val}
		 end,
		 S;

	     _ -> S
	 after calc_delay(S) ->
		 report_new_state(S#mystate{last_alive=add_5_sec(Last),
					    count_alive=(Count + 1) band 16#ffff},
				  S#mystate.di_alive, Count)
	 end,
    loop(NS).

set_dev(Pid, DI, V, Min, Max) ->
    Pid ! { self(), set, DI, V, Min, Max },
    receive
	ok -> ?ACNET_SUCCESS;
	{error, _} -> ?ACNET_INVARG
    after 100 -> ?ERR_READTMO
    end.

%%% Main loop for FSMSET.

devs(Bin) ->
    [{DI, V} || <<DI:32/little, V:16/little>> <= Bin].

fsmset(Pid) ->
    try
	receive
	    #acnet_request{ref=RpyId, mult=false,
			   data= <<10:16/little, Count:16/little, _Some:48,
				   Rest/binary>>}
	      when size(Rest) == Count * 6 ->
		%%info_msg("FSM Request: ~p~n Devs=~p~n",[Count,devs(Rest)]),
		lists:foreach(fun ({DI, V}) ->
				      set_dev(Pid, DI, V, -16#8000, 16#7fff)
			      end, devs(Rest)),
		acnet:send_last_reply(RpyId, ?ACNET_SUCCESS, <<>>)
		%%info_msg("FSMSET Reply value: ~p~n", [R]),
		    ;
	    #acnet_request{ref=RpyId, mult=true} = Req ->
		info_msg("FSMSET Bad request: ~p.~n", [Req]),
		acnet:send_last_reply(RpyId, ?ACNET_BADREQ, <<>>);

	    #acnet_request{ref=RpyId} = Req ->
		info_msg("FSMSET Unhandled request: ~p.~n", [Req]),
		acnet:send_last_reply(RpyId, ?ACNET_SYS, <<>>)
	end
    catch
	_:Any ->
	    info_msg("FSMSET caught: ~p.~n~p~n", [Any,erlang:get_stacktrace()])
    end,
    fsmset(Pid).

%%% Pulls the value of the "alive" states device's device index from
%%% the 'daq' app's environment. If no definition is there or the
%%% parameter is there but isn't an integer, then 'undefined' is
%%% returned.

get_alive_di() ->
    Param = states_alive_di,
    case application:get_env(daq, Param) of
	undefined ->
	    warning_msg("STATES FRONT-END: No keep alive states device configured. To~n"
			"remove this warning, add the parameter \"~p\" to the~n"
			"daq environment.~n", [Param]),
	    undefined;

	{ok, Val} ->
	    if
		is_integer(Val) ->
		    info_msg("STATES FRONT-END: Using ~B as the keep-alive state "
			     "device index~n", [Val]),
		    Val;

		true ->
		    warning_msg("STATES FRONT-END: Bad keep-alive device index "
				"specified. It~nshould be an integer but instead "
				"was defined as:~n   ~p~n", [Val]),
		    undefined
	    end
    end.

%%% Creates the table and enters an infinite loop.

init_task() ->
    {MSec, Sec, _} = os:timestamp(),
    {ok, S} = gen_udp:open(0),
    loop(#mystate{socket=S, di_alive=get_alive_di(), last_alive={MSec, Sec, 0}}).

init_fsmset(Pid) ->
    acnet:accept_requests(fsmset),
    fsmset(Pid).

set_states(#setdat_1device{di=DI, settingdata= <<Val:16/little>>,
			   ssdn= << _:32, Min:16/little, Max:16/little>>},
	   #aux_spec{shared=Pid}) ->
    set_dev(Pid, DI, Val, Min, Max).

read_states(#device_request{di=DI}, #aux_spec{shared=Pid, trigger=SE}) ->
    Pid ! { self(), read, DI },
    receive
	{ok, Val} ->
	    #device_reply{status=?ACNET_SUCCESS,
			  stamp = SE#sync_event.stamp,
			  data = <<Val:16/little>>};
	{error, _} ->
	    #device_reply{status=?ERR_UPDATE,
			  stamp = SE#sync_event.stamp,
			  data = <<0,0>>}
    after 100 ->
	    #device_reply{status=?ERR_READTMO,
			  stamp = os:timestamp(),
			  data = <<0,0>>}
    end.

set_status(#setdat_1device{di=DI, settingdata= <<Val:16/little>>},
	   #aux_spec{shared=Pid}) ->
    Pid ! { self(), enable, Val =:= 2, DI },
    receive
	ok -> ?ACNET_SUCCESS
    after 100 -> ?ERR_READTMO
    end.

bool_to_int(true) -> 2;
bool_to_int(false) -> 1.

read_status(#device_request{di=DI}, #aux_spec{shared=Pid, trigger=SE}) ->
    Pid ! { self(), status, DI },
    receive
	{ok, Val} ->
	    #device_reply{status=?ACNET_SUCCESS,
			  stamp = SE#sync_event.stamp,
			  data = <<(bool_to_int(Val)):16/little>>}
    after 100 ->
	    #device_reply{status=?ERR_READTMO,
			  stamp = os:timestamp(),
			  data = <<0,0>>}
    end.

start(Oid) ->
    acnet:start(fsmset, "FSMSET", "STATE"),
    Pid = spawn_link(states_dev, init_task,[]),
    spawn_link(states_dev, init_fsmset, [Pid]),
    Spec1 = #device_spec{ readf=fun read_states/2, setf=fun set_states/2,
			  atomic_bytes=?ATOMIC_SIZE, max_elements=1,
			  shared=Pid },
    Spec2 = #device_spec{ readf=fun read_status/2, setf=fun set_status/2,
			  atomic_bytes=?ATOMIC_SIZE, max_elements=1,
			  shared=Pid },
    devices:install_pi_attr_functions(Oid, ?PI_READNG, 0, Spec1),
    devices:install_pi_attr_functions(Oid, ?PI_SETTNG, 0, Spec1),
    devices:install_pi_attr_functions(Oid, ?PI_BASTAT, 0, Spec2),
    devices:install_pi_attr_functions(Oid, ?PI_BCNTRL, 0, Spec2).
