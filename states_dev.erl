%%% $Id: states_dev.erl,v 1.5 2011/10/05 14:10:04 nicklaus Exp $

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

-import(error_logger, [info_msg/1, info_msg/2]).

-define(ATOMIC_SIZE,2).

%%% Handle requests to the table.

loop(Tid, S, Seq) ->
    receive
	{ CPid, set, DI, Val, Min, Max } ->
            if 
              Val<Max andalso Val>Min ->
	        {MSec, Sec, USec} = now(),
	        CPid ! ok,
	        ets:insert(Tid, {DI, Val}),
	        Data = [<<1:16/little, Seq:16/little, 1:32/little>>,
		    <<0:16/little, DI:32/little, Val:16/little>>,
		    <<(MSec * 1000000 + Sec):32/little,
		      (USec * 1000):32/little>>],
	        acnet:send_usm(state, "STATES@STATES", Data),
	        loop(Tid, S, (Seq + 1) band 16#ffff);
              true ->
	        CPid ! {error, illegal_val},
	        loop(Tid, S, Seq)
            end;

	{ CPid, read, DI } ->
	    case ets:lookup(Tid, DI) of
		[] ->
		    CPid ! {error, not_found};
		[{_, Val}] ->
		    CPid ! {ok, Val}
	    end,
	    loop(Tid, S, Seq);

	_ ->
	    loop(Tid, S, Seq)
    end.

set_dev(Pid, DI, V, Min, Max) ->
    Pid ! { self(), set, DI, V, Min, Max },
    receive
	ok -> ?ACNET_SUCCESS;
	{error, _} -> ?ACNET_INVARG
    after 100 -> ?ERR_READTMO
    end.

%%% Main loop for FSMSET.

devs(Bin) ->
    [{DI, V} || <<DI:32/little, V:16/little>> <- Bin].

fsmset(Pid) ->
    catch receive
	      #acnet_request{ref=RpyId, mult=false,
			     data= <<10:16/little, Count:16/little, _:48,
				     Rest/binary>>}
		when size(Rest) == Count * 6 ->
		  lists:foreach(fun ({DI, V}) ->
					set_dev(Pid, DI, V, -16#8000, 16#7fff)
				end, devs(Rest)),
		  acnet:send_last_reply(RpyId, ?ACNET_SUCCESS, <<>>);

	      #acnet_request{ref=RpyId, mult=true} = Req ->
		  info_msg("Bad request: ~p.~n", [Req]),
		  acnet:send_last_reply(RpyId, ?ACNET_BADREQ, <<>>);

	      _ -> ok
	  end,
    fsmset(Pid).

%%% Creates the table and enters an infinite loop.

init_task() ->
    acnet:start(state),
    {ok, S} = gen_udp:open(0),
    loop(ets:new(stateDevTable, [set, private]), S, 0).

init_fsmset(Pid) ->
    acnet:start(fsmset),
    acnet:accept_requests(fsmset),
    fsmset(Pid).

set_states(#setdat_1device{di=DI, settingdata= <<Val:16/little>>,
			   ssdn= << _:32, Min:16/little, Max:16/little>>},
	   #aux_spec{shared=Pid}) ->
    set_dev(Pid, DI, Val, Min, Max).

read_states(#device_request{di=DI}, #aux_spec{shared=Pid,trigger=SE}) ->
    Pid ! { self(), read, DI },
    receive
	{ok, Val} ->
	    #device_reply{status=?ACNET_SUCCESS,
		  stamp = SE#sync_event.stamp,
		  data = <<Val:16/little>>};
	{error, not_found} ->
	    #device_reply{status=?ERR_UPDATE,
		  stamp = SE#sync_event.stamp,
		  data = <<0,0>>};
	{error, illegal_value} ->
	    #device_reply{status=?ERR_UPDATE,
		  stamp = SE#sync_event.stamp,
		  data = <<0,0>>}
    after 100 ->
	    #device_reply{status=?ERR_READTMO,
		  stamp = SE#sync_event.stamp,
		  data = <<0,0>>}
    end.

start(Oid) ->
    Pid = spawn_link(states_dev, init_task,[]),
    spawn_link(states_dev, init_fsmset, [Pid]),
    SpecA = #device_spec{ readf=fun read_states/2, setf=fun set_states/2,
			  atomic_bytes=?ATOMIC_SIZE, max_elements=1,
			  shared=Pid },
    devices:install_pi_attr_functions(Oid, ?PI_READNG, 0, SpecA),
    devices:install_pi_attr_functions(Oid, ?PI_SETTNG, 0, SpecA).
