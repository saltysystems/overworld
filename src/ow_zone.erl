-module(ow_zone).
-behaviour(gen_server).

% gen_server
-export([start/3, start/4]).
-export([start_link/3, start_link/4]).
-export([start_monitor/3, start_monitor/4]).
-export([call/2, call/3]).
-export([cast/2]).
-export([reply/2]).
-export([stop/1, stop/3]).

-export([
    join/3,
    part/3,
    disconnect/2,
    reconnect/2,
    rpc/4,
    broadcast/2,
    send/3
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%=======================================================================
%% Internal State, default configurations
%%=======================================================================

-define(TAG_I(Msg), {'$ow_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    zone_data :: zone_data()
}).

% DEFAULT_LERP_MS/DEFAULT_TICK_MS = 4 packets buffered by default
-define(DEFAULT_LERP_MS, 80).
-define(DEFAULT_TICK_MS, 20).

-define(INITIAL_ZONE_DATA, #{
    clients => [],
    frame => 0,
    tick_ms => ?DEFAULT_TICK_MS,
    lerp_period => ?DEFAULT_LERP_MS,
    disconnect => soft
}).

%%=======================================================================
%% Types
%%=======================================================================
-type server_name() :: gen_server:server_name().
-type server_ref() :: gen_server:server_ref().
-type start_opt() :: gen_server:start_opt().
-type start_ret() ::
    {ok, pid()}
    | ignore
    | {error, term()}.
-type start_mon_ret() ::
    {ok, {pid(), reference()}}
    | ignore
    | {error, term()}.
-type from() :: gen_server:from().
-type zone_msg() :: {atom(), map()}.
-type ow_zone_resp() ::
    noreply
    | {'@zone', zone_msg()}
    | {{'@', list()}, zone_msg()}.
-type zone_data() ::
    #{
        clients => [ow_session:id()],
        frame => non_neg_integer(),
        tick_ms => pos_integer(),
        lerp_period => pos_integer(),
        disconnect => hard | soft
    }.
-type state() :: #state{}.

%%=======================================================================
%% ow_zone callbacks
%%=======================================================================
-callback init(Args) -> Result when
    Args :: term(),
    Result ::
        {ok, InitialData}
        | {ok, InitialData, ConfigMap}
        | ignore
        | {stop, Reason},
    ConfigMap :: map(),
    InitialData :: term(),
    Reason :: term().

-callback handle_join(Msg, From, State) -> Result when
    From :: ow_session:id(),
    Msg :: term(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-callback handle_part(Msg, From, State) -> Result when
    From :: ow_session:id(),
    Msg :: term(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-callback handle_disconnect(From, State) -> Result when
    From :: ow_session:id(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-callback handle_reconnect(From, State) -> Result when
    From :: ow_session:id(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-callback handle_tick(ZoneData, State) -> Result when
    ZoneData :: zone_data(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-optional_callbacks([handle_join/3, handle_part/3, handle_disconnect/2]).

%%=======================================================================
%% gen_server API functions
%%=======================================================================

-spec start(Module, Args, Opts) -> Result when
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_ret().
start(Module, Args, Opts) ->
    gen_server:start(?MODULE, {Module, Args}, Opts).

-spec start(ServerName, Module, Args, Opts) -> Result when
    ServerName :: server_name(),
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_ret().
start(ServerName, Module, Args, Opts) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Opts).

-spec start_link(Module, Args, Opts) -> Result when
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_ret().
start_link(Module, Args, Opts) ->
    gen_server:start_link(?MODULE, {Module, Args}, Opts).

-spec start_link(ServerName, Module, Args, Opts) -> Result when
    ServerName :: server_name(),
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_ret().
start_link(ServerName, Module, Args, Opts) ->
    % TODO: Check for configuration options in Opts
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Opts).

-spec start_monitor(Module, Args, Opts) -> Result when
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_mon_ret().
start_monitor(Module, Args, Opts) ->
    gen_server:start_monitor(?MODULE, {Module, Args}, Opts).

-spec start_monitor(ServerName, Module, Args, Opts) -> Result when
    ServerName :: server_name(),
    Module :: module(),
    Args :: term(),
    Opts :: [start_opt()],
    Result :: start_mon_ret().
start_monitor(ServerName, Module, Args, Opts) ->
    gen_server:start_monitor(ServerName, ?MODULE, {Module, Args}, Opts).

-spec call(ServerRef, Message) -> Reply when
    ServerRef :: server_ref(),
    Message :: term(),
    Reply :: term().
call(ServerRef, Msg) ->
    gen_server:call(ServerRef, Msg).

-spec call(ServerRef, Message, Timeout) -> Reply when
    ServerRef :: server_ref(),
    Message :: term(),
    Timeout :: timeout(),
    Reply :: term().
call(ServerRef, Msg, Timeout) ->
    gen_server:call(ServerRef, Msg, Timeout).

-spec cast(ServerRef, Message) -> ok when
    ServerRef :: server_ref(),
    Message :: term().
cast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, Msg).

-spec reply(From, Message) -> ok when
    From :: from(),
    Message :: term().
reply(From, Reply) ->
    gen_server:reply(From, Reply).

-spec stop(ServerRef) -> ok when
    ServerRef :: server_ref().
stop(ServerRef) ->
    gen_server:stop(ServerRef).

-spec stop(ServerRef, Reason, Timeout) -> ok when
    ServerRef :: server_ref(),
    Reason :: term(),
    Timeout :: timeout().
stop(ServerRef, Reason, Timeout) ->
    gen_server:stop(ServerRef, Reason, Timeout).

%%=======================================================================
%% Public API for ow_zone
%%=======================================================================

-spec join(server_ref(), term(), ow_session:id()) -> ok.
join(ServerRef, Msg, SessionID) ->
    gen_server:call(ServerRef, ?TAG_I({join, Msg, SessionID})).

-spec part(server_ref(), term(), ow_session:id()) -> ok.
part(ServerRef, Msg, SessionID) ->
    gen_server:call(ServerRef, ?TAG_I({part, Msg, SessionID})).

-spec disconnect(server_ref(), ow_session:id()) -> ok.
disconnect(ServerRef, SessionID) ->
    gen_server:cast(ServerRef, ?TAG_I({disconnect, SessionID})).

-spec reconnect(server_ref(), ow_session:id()) -> ok.
reconnect(ServerRef, SessionID) ->
    gen_server:cast(ServerRef, ?TAG_I({reconnect, SessionID})).

-spec rpc(server_ref(), atom(), term(), ow_session:id()) -> ok.
rpc(ServerRef, Type, Msg, SessionID) ->
    gen_server:call(ServerRef, ?TAG_I({rpc, Type, Msg, SessionID})).

-spec broadcast(server_ref(), term()) -> ok.
broadcast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, ?TAG_I({broadcast, Msg})).

-spec send(server_ref(), [ow_session:id()], term()) -> ok.
send(ServerRef, IDs, Msg) ->
    gen_server:cast(ServerRef, ?TAG_I({send, IDs, Msg})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback functions for internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Initialize the internal state of the zone, with timer
init({CbMod, CbArgs}) ->
    init(CbMod, CbMod:init(CbArgs)).
init(CbMod, {ok, CbData, ZoneData}) ->
    Config = maps:merge(?INITIAL_ZONE_DATA, ZoneData),
    St0 = initialize_state(CbMod, CbData, Config),
    {ok, St0};
init(CbMod, {ok, CbData}) ->
    Config = ?INITIAL_ZONE_DATA,
    St0 = initialize_state(CbMod, CbData, Config),
    {ok, St0};
init(_CbMod, ignore) ->
    ignore;
init(_CbMod, Stop) ->
    Stop.

handle_call(?TAG_I({join, Msg, Who}), From, #state{zone_data = ZD} = St0) ->
    % Check the callback module for a handle_join function
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    State1 =
        case erlang:function_exported(CbMod, handle_join, 3) of
            true ->
                {Notify, CbData1} = CbMod:handle_join(Msg, Who, CbData0),
                St1 = St0#state{cb_data = CbData1},
                % Notify the caller of any messages that the callback module
                % wants to send as a response to the joint
                handle_notify(Notify, St1),
                % TODO: maybe set a termination callback?
                St1;
            false ->
                % Assume the callback module does not want to take any
                % additional action
                St0
        end,
    #{clients := Clients} = ZD,
    ZD1 = ZD#{clients := [Who | Clients]},
    % Update the session with zone information
    ow_session:zone(From, Who),
    {reply, ok, State1#state{zone_data = ZD1}};
handle_call(?TAG_I({part, Msg, Who}), _From, St0) ->
    St1 = handle_part_inner(Msg, Who, St0),
    {reply, ok, St1};
handle_call(?TAG_I({Type, Msg, SessionID}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    Handler = list_to_existing_atom("handler_" ++ atom_to_list(Type)),
    {Notify, CbData1} =
        maybe
            true ?= erlang:function_exported(CbMod, Handler, 3),
            CbMod:Handler(Msg, SessionID, CbData)
        else
            false ->
                {noreply, ok, CbData}
        end,
    St1 = St0#state{cb_data = CbData1},
    % Send any messages as needed - called for side effects
    handle_notify(Notify, St1),
    {reply, ok, St1};
handle_call(_Call, _From, St0) ->
    %TODO : Allow fall-through ?
    {reply, ok, St0}.

handle_cast(
    ?TAG_I({disconnect, Who}),
    St0 = #state{zone_data = #{disconnect := hard}}
) ->
    % Run the part handler instead of disconnect
    St1 = handle_part_inner(#{}, Who, St0),
    {noreply, ok, St1};
handle_cast(
    ?TAG_I({disconnect, Who}),
    St0 = #state{zone_data = #{disconnect := soft}}
) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    NextState =
        case erlang:function_exported(CbMod, handle_disconnect, 2) of
            true ->
                {Notify, CbData1} = CbMod:handle_disconnect(Who, CbData0),
                St1 = St0#state{cb_data = CbData1},
                % Notify the caller of any messages that the callback module
                % wants to send as a response to the joint
                handle_notify(Notify, St1),
                St1;
            false ->
                % Assume the callback module does not want to take any
                % additional action
                St0
        end,
    {noreply, NextState};
% Note: We guard for only the soft disconnect because reconnecting with a hard
% disconnect is considered an error.
handle_cast(
    ?TAG_I({reconnect, Who}),
    St0 = #state{zone_data = #{disconnect := soft}}
) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    NextState =
        case erlang:function_exported(CbMod, handle_reconnect, 2) of
            true ->
                {Notify, CbData1} = CbMod:handle_reconnect(Who, CbData0),
                St1 = St0#state{cb_data = CbData1},
                % Notify the caller of any messages that the callback module
                % wants to send as a response to the joint
                handle_notify(Notify, St1),
                St1;
            false ->
                % Assume the callback module does not want to take any
                % additional action
                St0
        end,
    {noreply, NextState};
handle_cast(?TAG_I({broadcast, Msg}), St0) ->
    handle_notify({'@zone', Msg}, St0),
    {noreply, St0};
handle_cast(?TAG_I({send, IDs, Msg}), St0) ->
    handle_notify({{'@', IDs}, Msg}, St0),
    {noreply, St0};
handle_cast(_Cast, St0) ->
    %TODO : Allow fall-through ?
    {noreply, St0}.

handle_info(?TAG_I(tick), St0) ->
    St1 = tick(St0),
    {noreply, St1};
handle_info(Msg, #state{cb_mod = CbMod} = St0) ->
    St1 =
        case erlang:function_exported(CbMod, handle_info, 2) of
            false ->
                St0;
            true ->
                info(Msg, St0)
        end,
    {noreply, St1}.

terminate(_Reason, _St0) -> ok.
code_change(_OldVsn, St0, _Extra) -> {ok, St0}.

%%=======================================================================
%% Internal functions
%%=======================================================================

-spec tick(state()) -> state().
tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0, zone_data = ZoneData}) ->
    {Notify, CbData1} = CbMod:handle_tick(ZoneData, CbData0),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    St1.

info(Msg, St0) ->
    #state{cb_mod = CbMod, cb_data = CbData} = St0,
    {Notify, CbData1} = CbMod:handle_info(Msg, CbData),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    St1.

handle_part_inner(Msg, Who, #state{zone_data = ZD} = St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    NextState =
        case erlang:function_exported(CbMod, handle_part, 3) of
            true ->
                {Notify, CbData1} = CbMod:handle_part(Msg, Who, CbData0),
                St1 = St0#state{cb_data = CbData1},
                % Send any messages as needed - called for side effects
                handle_notify(Notify, St1),
                St1;
            false ->
                % Assume the callback module does not want to take any
                % additional action
                St0
        end,
    #{clients := Clients} = ZD,
    Clients1 = lists:delete(Who, Clients),
    ZD1 = ZD#{clients := Clients1},
    NextState#state{zone_data = ZD1}.

handle_notify({{'@', IDs}, {MsgType, Msg}}, #state{zone_data = ZD}) ->
    #{clients := Clients} = ZD,
    % Notify clients that are both in the list to be notified AND a current client
    Players = [P0 || P0 <- IDs, P1 <- Clients, P0 =:= P1],
    ow_session_util:notify_clients(MsgType, Msg, Players);
handle_notify({'@zone', {MsgType, Msg}}, #state{zone_data = ZD}) ->
    #{clients := Clients} = ZD,
    % SEND MESSAGE: Send everyone the message
    ow_session_util:notify_clients(MsgType, Msg, Clients);
handle_notify(noreply, _St0) ->
    % NO MESSAGE
    ok.

initialize_state(CbMod, CbData, ZoneData) ->
    #{tick_ms := TickMs} = ZoneData,
    % setup the timer
    timer:send_interval(TickMs, self(), ?TAG_I(tick)),
    #state{
        cb_mod = CbMod,
        cb_data = CbData,
        zone_data = ZoneData
    }.
