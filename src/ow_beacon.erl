-module(ow_beacon).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

% Public API
-export([start/0, stop/0, last/0, get_by_id/1, dump/0]).

% ow callbacks
-export([beacon/1]).

% genserver callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% milliseconds
-define(HEARTBEAT, 1000).
% The number of beacons to hold in memory
-define(WINDOW_SIZE, 25).

-rpc_encoder(overworld_pb).
-rpc_client([session_beacon]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    gen_server:stop(?SERVER),
    ok.

last() ->
    gen_server:call(?MODULE, last).

get_by_id(ID) ->
    gen_server:call(?MODULE, {get_by_id, ID}).

dump() ->
    gen_server:call(?MODULE, dump).

% It makes no sense for the client to send beacons to the server, just throw
% away these messages
beacon(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% genserver callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, new_state()}.

handle_call({get_by_id, ID}, _From, State = {Window, _Timer}) ->
    case lists:keyfind(ID, 1, Window) of
        false ->
            io:format("Don't have this ID: ~p~n", [ID]),
            io:format("Window is : ~p~n", [Window]),
            {reply, 0, State};
        {ID, Time} ->
            {reply, Time, State}
    end;
handle_call(last, _From, {[Now | _Rest], Timer}) ->
    {reply, Now, {Now, Timer}};
handle_call(dump, _From, {Window, Timer}) ->
    {reply, Window, {Window, Timer}};
handle_call(_Call, _From, State) ->
    {reply, State, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(tick, {Window, OldTimer}) ->
    % Stop the old timer
    erlang:cancel_timer(OldTimer),
    % Create a new unique integer as the beacon ID
    % TODO: SEED ME
    % 32-bit unsigned max
    ID = rand:uniform(4_294_967_295),
    % Push the beacon ID + current time to the stack
    Beacon = {ID, erlang:monotonic_time()},
    % Encode the beacon and broadcast it to all clients.
    % n.b., There's a very small race here. A client could conceivably respond
    % faster than the server has updated the list but I've never observed this
    % even on LAN
    % Look up the QOS/Channel for the beacon
    #{channel := Channel, qos := QOS} = ow_protocol:rpc(
        session_beacon, client
    ),
    ow_session:broadcast(encode_beacon(ID), {QOS, Channel}),
    NewTimer = erlang:send_after(?HEARTBEAT, self(), tick),
    {noreply, {push(Beacon, Window), NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_state() ->
    ID = erlang:unique_integer(),
    Now = erlang:monotonic_time(),
    NewTimer = erlang:send_after(?HEARTBEAT, self(), tick),
    {[{ID, Now}], NewTimer}.

% Push the latest beacon onto a fixed length list of the last ?WINDOW_SIZE
% number of beacons.
push(Beacon, List) ->
    L1 = lists:sublist(List, ?WINDOW_SIZE - 1),
    [Beacon | L1].

-spec encode_beacon(integer()) -> binary().
encode_beacon(ID) ->
    ow_msg:encode(#{id => ID}, session_beacon).
