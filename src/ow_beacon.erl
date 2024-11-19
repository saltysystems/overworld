%%%-------------------------------------------------------------------
%% @doc Overworld latency measurement and WebSocket connection 
%%      keep-alive
%% @end
%%%-------------------------------------------------------------------

-module(ow_beacon).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

% Public API
-export([start/0, stop/0, last/0, get_by_id/1, dump/0]).

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

-rpc_encoder(#{app => overworld, lib => overworld_pb, interface => ow_msg}).
-rpc_client([session_beacon]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------  
%% @doc Start the Beacon service. This service keeps track of client 
%%      latencies.
%% @end  
%%-------------------------------------------------------------------------
-spec start() -> gen_server:start_ret().
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------------  
%% @doc Stop the beacon service.
%% @end  
%%-------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER),
    ok.

%%-------------------------------------------------------------------------  
%% @doc  ?
%% @end  
%%-------------------------------------------------------------------------
-spec last() -> integer().
last() ->
    gen_server:call(?MODULE, last).

%%-------------------------------------------------------------------------  
%% @doc Retrieve the timestamp of a beacon by its ID
%% @end  
%%-------------------------------------------------------------------------
-spec get_by_id(integer()) -> integer().
get_by_id(ID) ->
    gen_server:call(?MODULE, {get_by_id, ID}).

-spec dump() -> [integer()].
dump() ->
    gen_server:call(?MODULE, dump).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% genserver callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, new_state()}.

handle_call({get_by_id, ID}, _From, State = {Window, _Timer}) ->
    case lists:keyfind(ID, 1, Window) of
        false ->
            logger:notice("Don't have this ID: ~p~n", [ID]),
            logger:notice("Window is : ~p~n", [Window]),
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
    %[H|_] = Window,
    %logger:notice("Beacon tick: ~p", [H]),
    % Stop the old timer
    erlang:cancel_timer(OldTimer),
    % Create a new unique integer as the beacon ID
    ID = erlang:unique_integer([positive]),
    % Push the beacon ID + current time to the stack
    Beacon = {ID, erlang:monotonic_time()},
    % Encode the beacon and broadcast it to all clients.
    Msg = {session_beacon, #{id => ID}},
    Clients = pg:get_members(overworld, clients),
    ow_session_util:notify_clients(Msg, Clients),
    % Start next timer
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
