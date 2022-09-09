%%% $Id: states_dev.erl,v 1.6 2012/06/15 20:32:50 neswold Exp $

%%% States Front-end Driver
%%%
%%% This device driver implements a stripped down STATES facility
%%% similar to the one in the real control system. This driver is
%%% simpler in that it only supports multicasting state changes due to
%%% a setting. If necessary, we can add the other complexity.

-module(states_dev).
-vsn(1).
-behavior(driver).

-include_lib("daq/include/devices.hrl").

-export([init/1, message/2, reading/4, setting/3, terminate/1]).

-import(error_logger, [info_msg/1, info_msg/2, warning_msg/2]).

-record(dev_state, {'table', 'socket', seq=0}).

-type dev_state() :: #dev_state{table :: ets:table(),
				socket :: gen_udp:socket(),
				seq :: pos_integer()}.

%%% ----------------------------------------------------------------
%%% The functions in this section are responsible for querying and
%%% updating the state device table.
%%% ----------------------------------------------------------------

-spec update_status(ets:tid(), integer(), boolean()) -> 'true'.

update_status(Tid, DI, State) ->
    case ets:lookup(Tid, DI) of
	[] -> 'true';
	[{_, Val, _}] ->
	    ets:insert(Tid, {DI, Val, State})
    end.

-spec update_value(ets:tid(), integer(), integer()) -> boolean().

update_value(Tid, DI, Val) ->
    case ets:lookup(Tid, DI) of
	[] ->
	    ets:insert(Tid, {DI, Val, 'true'}),
	    'false';

	[{_, _, State}] ->
	    ets:insert(Tid, {DI, Val, State}),
	    State
    end.

-spec read_state(ets:tid(), integer()) -> boolean().

read_state(Tid, DI) ->
    case ets:lookup(Tid, DI) of
	[] -> 'false';
	[{_, _, State}] -> State
    end.

-spec read_value(ets:tid(), integer()) ->
			{acnet:status(), integer()}.

read_value(Tid, DI) ->
    case ets:lookup(Tid, DI) of
	[] ->
	    {?ERR_UPDATE, 0};
	[{_, Val, _}] ->
	    {?ACNET_SUCCESS, Val}
    end.

%%% Pulls the value of the "alive" states device's device index from
%%% the 'daq' app's environment. If no definition is there or the
%%% parameter is there but isn't an integer, then 'undefined' is
%%% returned.

-spec get_alive_di() -> 'undefined' | integer().

get_alive_di() ->
    Param = 'states_alive_di',
    case application:get_env('daq', Param) of
	'undefined' ->
	    warning_msg("STATES FRONT-END: No keep-alive states device "
			"configured. To~nremove this warning, add the "
			"parameter \"~p\" to the~n'daq' environment.~n",
			[Param]),
	    'undefined';

	{'ok', Val} when is_integer(Val) ->
	    info_msg("STATES FRONT-END: Using ~B as the keep-alive "
		     "state device index.~n", [Val]),
	    Val;

	{'ok', Val} ->
	    warning_msg("STATES FRONT-END: Bad keep-alive device "
			"index specified. It~nshould be an integer "
			"but instead was defined as:~n   ~p~n", [Val]),
	    'undefined'
    end.

%%% Multicast a STATES protocol message.

-spec report_new_state(dev_state(), integer(), integer()) -> dev_state().

report_new_state(#dev_state{table=Tid, seq=Seq} = S, DI, Val) ->
    {MSec, Sec, USec} = os:timestamp(),
    case update_value(Tid, DI, Val) of
	'true' ->
	    Data = <<1:16/little, Seq:16/little, 1:32/little,
		     0:16/little, DI:32/little, Val:16/little,
		     (MSec * 1000000 + Sec):32/little,
		     (USec * 1000):32/little>>,
	    acnet:send_usm('fsmset', "STATES@STATES", Data),
	    S#dev_state{seq=(Seq + 1) band 16#ffff};

	'false' ->
	    S
    end.

-spec devs(binary()) -> {integer(), integer()}.

devs(Bin) ->
    [{DI, V} || <<DI:32/little, V:16/little>> <= Bin].

%%% ----------------------------------------------------------------
%%% This section defines the 'driver' behavior interface.
%%% ----------------------------------------------------------------

%%% Initialize the driver's state and the resources it uses.

-spec init(any()) ->
		  {'ready', dev_state(), array:array()} |
		  {'error', string()}.

init(_) ->
    {'ok', S} = gen_udp:open(0),
    try

	%% Create the FSMSET handle and enable it for receiving
	%% requests.

	acnet:start('fsmset', "FSMSET", "STATE"),
	acnet:accept_requests('fsmset'),

	%% Create the table that will hold the state of the state
	%% devices.

	Tid = ets:new('stateDevTable', ['set', 'private']),

	%% Build the ACSys/FE framework attribute list. This driver
	%% has only two attributes for its devices.

	Attrs = [#attr_spec{typ_elem='Int16', num_elem=1, name='state',
			    description= <<"Read/set a state device.">>},
		 #attr_spec{typ_elem='Int16', num_elem=1, name='status',
			    description= <<"Read/set a state device's status.">>}],

	%% If the configuration defined a "keep alive" state device,
	%% add it to the table (with an initial value of 0) and start
	%% a 5 second, periodic timer to update it.

	case get_alive_di() of
	    'undefined' -> 'ok';
	    DI ->
		update_value(Tid, DI, 0),
		timer:send_interval(5000, {'timeout', DI})
	end,

	%% Return a success result to the framework.

	{'ready', #dev_state{socket=S, table=Tid}, array:from_list(Attrs)}
    catch
	_:_ ->
	    gen_udp:close(S),
	    {'error', "Error connecting to ACNET."}
    end.

%%% Local helper function to conevrt a boolean to a 16-bit binary.

bool_to_bin('true') -> <<1:16/little>>;
bool_to_bin('false') -> <<0:16/little>>.

%%% This function is called by the framework when reading the `state`
%%% attribute.

reading(#dev_state{table=Table} = S, _, #reading_context{attribute='state', di=DI},
	#sync_event{stamp=Stamp}) ->

    %% Retrieve the value associated with the given device index along
    %% with the result status of the look-up.

    {Status, Data} = read_value(Table, DI),

    %% Return our state (it wasn't updated) along with the reply
    %% record for the request.

    {S, #device_reply{stamp=Stamp, data= <<Data:16/little>>, status=Status}};

%%% This function is executed when the framework wants to read the
%%% `status` attribute.

reading(#dev_state{table=Table} = S, _, #reading_context{attribute='status', di=DI},
	#sync_event{stamp=Stamp}) ->
    {S, #device_reply{stamp=Stamp, status=?ACNET_SUCCESS,
		      data=bool_to_bin(read_state(Table, DI))}}.

%%% This function is called when handling setting requests in the
%%% framework for the `state` attribute.

setting(S, #setting_context{ssdn= <<_:32, Mn:16/little, Mx:16/little>>, di=DI,
			    attribute='state'}, <<V:16/little>>) ->
    set_dev(S, DI, V, Mn, Mx);

%%% This function is called when handling setting requests in the
%%% framework for the `status` attribute.

setting(#dev_state{table=Table} = S, #setting_context{attribute='status', di=DI},
	<<Val:16/little>>) ->

    %% Update the enable/disable state of the device. If the settings
    %% is 2, the device gets enabled. Otherwise it's disabled.

    update_status(Table, DI, Val =:= 2),
    {S, ?ACNET_SUCCESS}.

terminate(#dev_state{socket=Sock}) ->
    gen_udp:close(Sock).

%%% This callback handles miscellaneous messages that we configured to
%%% be sent to us.

-spec message(dev_state(), term()) -> dev_state().

message(S, #acnet_request{data= <<10:16/little, Count:16/little, _Some:48,
				  Rest/binary>>, ref=RpyId, mult='false'})
  when size(Rest) == Count * 6 ->
    acnet:send_last_reply(RpyId, ?ACNET_SUCCESS, <<>>),
    lists:foldl(fun ({DI, V}, Acc) ->
			{NAcc, _} = set_dev(Acc, DI, V, -16#8000, 16#7fff),
			NAcc
		end, S, devs(Rest));

%%% No FSMSET request need multiple replies. Return a BAD REQUEST
%%% status.

message(S, #acnet_request{ref=RpyId, mult='true'} = Req) ->
    info_msg("FSMSET Bad request: ~p.~n", [Req]),
    acnet:send_last_reply(RpyId, ?ACNET_BADREQ, <<>>),
    S;

%%% If we get a timeout message, it's time to send out the "keep
%%% alive" state.

message(#dev_state{table=Tid} = S, {'timeout', DI}) ->
    {_, Count} = read_value(Tid, DI),
    report_new_state(S, DI, (Count + 1) band 16#ffff);

%%% Report any other type of request.

message(S, #acnet_request{ref=RpyId} = Req) ->
    info_msg("FSMSET Unhandled request: ~p.~n", [Req]),
    acnet:send_last_reply(RpyId, ?ACNET_SYS, <<>>),
    S.

-spec set_dev(dev_state(), integer(), integer(), integer(), integer()) ->
		     {dev_state(), acnet:status()}.

set_dev(S, _, V, Min, _) when V < Min ->
    { S, ?ACNET_INVARG };
set_dev(S, _, V, _, Max) when V > Max ->
    { S, ?ACNET_INVARG };
set_dev(S, DI, V, _, _) ->
    { report_new_state(S, DI, V), ?ACNET_SUCCESS }.
