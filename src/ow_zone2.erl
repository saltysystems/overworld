-module(ow_zone2).
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
    who/1,
    status/1,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG_I(Msg), {'$ow_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    zone_data :: zone_data()
}).

% DEFAULT_LERP_MS/DEFAULT_TICK_MS = 4 packets buffered by default
-define(DEFAULT_LERP_MS, 80).
-define(DEFAULT_TICK_MS, 20).
-define(DEFAULT_DC_TIMEOUT_MS, 3000).

-define(INITIAL_ZONE_DATA, #{
    clients => [],
    disconnects => [],
    mobs => [],
    garbage => [],
    frame => 0,
    tick_ms => ?DEFAULT_TICK_MS,
    lerp_period => ?DEFAULT_LERP_MS,
    dc_timeout_ms => ?DEFAULT_DC_TIMEOUT_MS
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
-type session() :: ow_session:session().
-type session_id() :: integer().
-type zone_msg() :: {atom(), map()}.
-type ow_zone_resp() ::
    noreply
    | {'@zone', zone_msg()}
    | {{'@', list()}, zone_msg()}.
-type zone_data() ::
    #{
        clients => [session()],
        disconnects => [session()],
        npcs => [session()],
        garbage => [session_id()],
        frame => non_neg_integer(),
        tick_ms => pos_integer(),
        lerp_period => pos_integer(),
        dc_timeout_ms => pos_integer()
    }.
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ow_zone callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

-callback handle_join(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    PlayerInfo :: any(),
    Status :: atom() | {ok, Session} | {ok, Session, PlayerInfo},
    Response :: ow_zone_resp().

-callback handle_part(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().

-callback handle_disconnect(Session, State) -> Result when
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().

-callback handle_tick(ZoneData, State) -> Result when
    ZoneData :: zone_data(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public behaviour API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec join(server_ref(), term(), session()) -> {ok, session()}.
join(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({join, Msg, Session})).

-spec part(server_ref(), term(), session()) -> {ok, session()}.
part(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({part, Msg, Session})).

-spec disconnect(server_ref(), session()) -> {ok, session()}.
disconnect(ServerRef, Session) ->
    gen_server:call(ServerRef, ?TAG_I({disconnect, Session})).

-spec reconnect(server_ref(), session()) -> ok.
reconnect(ServerRef, Session) ->
    gen_server:cast(ServerRef, ?TAG_I({reconnect, Session})).

-spec rpc(server_ref(), atom(), term(), session()) -> {ok, session()}.
rpc(ServerRef, Type, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({rpc, Type, Msg, Session})).

-spec who(server_ref()) -> list().
who(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I(who)).

-spec status(server_ref()) -> undefined | term().
status(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I(status)).

-spec broadcast(server_ref(), term()) -> ok.
broadcast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, ?TAG_I({broadcast, Msg})).

-spec send(server_ref(), [session_id()], term()) -> ok.
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

initialize_state(CbMod, CbData, ZoneData) ->
    #{tick_ms := TickMs} = ZoneData,
    %#{tick_ms := TickMs, dc_timeout_ms := DCTimeoutMs} = Config,
    %ZoneData = #{tick_ms => TickMs},
    % setup the timer
    timer:send_interval(TickMs, self(), ?TAG_I(tick)),
    #state{
        cb_mod = CbMod,
        cb_data = CbData,
        zone_data = ZoneData
    }.

handle_call(?TAG_I({join, Msg, Session}), _From, St0) ->
    {Session1, St1} = handle_join(Msg, Session, St0),
    % Get channel and QOS information
    {reply, {ok, Session1, enet_msg_opts(join)}, St1};
handle_call(?TAG_I({part, Msg, Session}), _From, St0) ->
    {Session1, St1} = handle_part(Msg, Session, St0),
    % Get channel and QOS information
    {reply, {ok, Session1, enet_msg_opts(part)}, St1};
handle_call(?TAG_I(who), _From, St0) ->
    Players = ow_player_reg:list(self()),
    IDs = [ow_player_reg:get_id(P) || P <- Players],
    {reply, IDs, St0};
handle_call(?TAG_I({Type, Msg, Session}), _From, St0) ->
    {Session1, St1} = handle_rpc(Type, Msg, Session, St0),
    {reply, {ok, Session1, enet_msg_opts(Type)}, St1};
handle_call(?TAG_I({disconnect, Session}), _From, St0) ->
    ZoneData = St0#state.zone_data,
    #{disconnects := Disconnects} = ZoneData,
    {Session1, St1} = notify_disconnect(Session, St0),
    Now = erlang:monotonic_time(),
    Disconnects1 = [{Session1, Now} | Disconnects],
    St2 = St1#state{zone_data = ZoneData#{disconnects => Disconnects1}},
    {reply, {ok, Session1}, St2};
handle_call(_Call, _From, St0) ->
    %TODO : Allow fall-through ?
    {reply, ok, St0}.

handle_cast(?TAG_I({reconnect, Session}), St0) ->
    ZoneData = St0#state.zone_data,
    #{disconnects := Disconnects} = ZoneData,
    % Filter out the pending disconnect
    ID = ow_session:get_id(Session),
    F = fun({PendingSession, When}, Acc) ->
        PendingID = ow_session:get_id(PendingSession),
        case PendingID == ID of
            true ->
                Acc;
            false ->
                [{PendingSession, When} | Acc]
        end
    end,
    Disconnects1 = lists:foldl(F, [], Disconnects),
    St1 = St0#state{zone_data = ZoneData#{disconnects => Disconnects1}},
    {noreply, St1};
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
    St2 = timeout_disconnects(St1),
    {noreply, St2};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec tick(state()) -> state().
tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0, zone_data = ZoneData}) ->
    #{tick_ms := TickMs} = ZoneData,
    {Notify, CbData1} = CbMod:handle_tick(TickMs, CbData0),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    St1.

-spec timeout_disconnects(state()) -> state().
timeout_disconnects(St0) ->
    ZoneData = St0#state.zone_data,
    #{disconnects := Disconnects, dc_timeout_ms := DCTimeoutMs} = ZoneData,
    Now = erlang:monotonic_time(),
    F =
        fun(P = {Session, When}, State) ->
            Delta = erlang:convert_time_unit(
                (Now - When), native, millisecond
            ),
            case Delta > DCTimeoutMs of
                true ->
                    ID = ow_session:get_id(Session),
                    logger:notice("Disconnecting ~p: timeout", [ID]),
                    Disconnects1 = lists:delete(P, Disconnects),
                    % The callback part handler MUST accept an empty message as a result
                    {_Session1, St1} = handle_part(#{}, Session, St0),
                    ZD1 = ZoneData#{disconnects => Disconnects1},
                    St1#state{zone_data = ZD1};
                false ->
                    State
            end
        end,
    lists:foldl(F, St0, Disconnects).

info(Msg, St0) ->
    #state{cb_mod = CbMod, cb_data = CbData} = St0,
    {Notify, CbData1} = CbMod:handle_info(Msg, CbData),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, CbData1),
    St1.

handle_notify({{'@', IDs}, {MsgType, Msg}}, _St0) ->
    % SEND MESSAGE: Filter just for the players we want to notify
    Players = [ow_player_reg:get(ID) || ID <- IDs],
    case Players of
        [] ->
            % Nothing to send
            ok;
        Players ->
            notify_players(MsgType, Msg, Players)
    end;
handle_notify({'@zone', {MsgType, Msg}}, _St0) ->
    % SEND MESSAGE: Send everyone the message
    Players = ow_player_reg:list(self()),
    case Players of
        [] ->
            % Nothing to send
            ok;
        Players ->
            notify_players(MsgType, Msg, Players)
    end;
handle_notify(noreply, _St0) ->
    % NO MESSAGE
    ok.

player_add(PlayerInfo, Session) ->
    % Add the player to the list of players.
    ID = ow_session:get_id(Session),
    PID = ow_session:get_pid(Session),
    Serializer = ow_session:get_serializer(Session),
    % Force a crash via failed match if player registration doesn't go well
    ok = ow_player_reg:new(ID, PID, Serializer, self(), PlayerInfo).

player_rm(Session) ->
    ID = ow_session:get_id(Session),
    ow_player_reg:delete(ID).

notify_players(MsgType, Msg, Players) ->
    Send = fun
        ({error, _}) ->
            ok;
        (Player) ->
            PID = ow_player_reg:get_pid(Player),
            Serializer = ow_player_reg:get_serializer(Player),
            case PID of
                undefined ->
                    ok;
                Pid ->
                    case Serializer of
                        undefined ->
                            Pid ! {self(), zone_msg, {MsgType, Msg}};
                        protobuf ->
                            #{
                                channel := Channel,
                                qos := QOS,
                                encoder := Encoder
                            } =
                                ow_protocol:rpc(MsgType, client),
                            #{
                                interface := EncoderMod,
                                app := App,
                                lib := EncoderLib
                            } = Encoder,
                            EncodedMsg = erlang:apply(EncoderMod, encode, [
                                Msg, MsgType, EncoderLib, App
                            ]),
                            Pid !
                                {
                                    self(),
                                    zone_msg,
                                    EncodedMsg,
                                    {QOS, Channel}
                                }
                    end
            end
    end,
    lists:foreach(Send, Players).

handle_join(Msg, Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    {Notify, Status, CbData1} = CbMod:handle_join(Msg, Session, CbData0),
    Session1 =
        case Status of
            {ok, S1, PlayerInfo} ->
                player_add(PlayerInfo, S1),
                S1;
            {ok, S1} ->
                player_add(undefined, S1),
                S1;
            _ ->
                player_add(undefined, Session),
                Session
        end,
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    % Set the player's termination callback
    Session2 =
        ow_session:set_termination_callback(
            {CbMod, disconnect, 1}, Session1
        ),
    {Session2, St1}.

handle_part(Msg, Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    {Notify, Status, CbData1} = CbMod:handle_part(Msg, Session, CbData0),
    Session1 = update_session(Status, Session),
    player_rm(Session1),
    % Send any messages as needed - called for side effects
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    {Session1, St1}.

handle_rpc(Type, Msg, Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    SessionID = ow_session:get_id(Session),
    % Overworld will confirm that the player is actually part of the zone to
    % which they are sending RPCs
    Handler = list_to_existing_atom("handler_" ++ atom_to_list(Type)),
    {Notify, Status, CbData1} =
        maybe
            true ?= is_player(SessionID),
            true ?= erlang:function_exported(CbMod, Handler, 3),
            CbMod:Handler(Msg, Session, CbData)
        else
            false ->
                {noreply, ok, CbData}
        end,
    Session1 = update_session(Status, Session),
    St1 = St0#state{cb_data = CbData1},
    % Send any messages as needed - called for side effects
    handle_notify(Notify, St1),
    {Session1, St1}.

-spec notify_disconnect(session(), term()) -> {session(), term()}.
notify_disconnect(Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    ID = ow_session:get_id(Session),
    case ow_player_reg:get(ID) of
        {error, _} ->
            % player is already disconnected
            {Session, St0};
        _ ->
            {Notify, Status, CbData1} = CbMod:handle_disconnect(
                Session, CbData
            ),
            Session1 = update_session(Status, Session),
            St1 = St0#state{cb_data = CbData1},
            handle_notify(Notify, St1),
            {Session1, St1}
    end.

-spec is_player(session_id()) -> boolean().
is_player(ID) ->
    PlayerList = ow_player_reg:list(self()),
    PIDs = [ow_player_reg:get_id(P) || P <- PlayerList],
    lists:member(ID, PIDs).

-spec update_player(term(), session_id()) -> ok.
update_player(PlayerInfo, ID) ->
    P = ow_player_reg:get(ID),
    P1 = ow_player_reg:set_info(PlayerInfo, P),
    ow_player_reg:update(P1).

-spec enet_msg_opts(atom()) ->
    {atom(), non_neg_integer()}.
enet_msg_opts(Action) ->
    #{channel := Channel, qos := QOS} = ow_protocol:rpc(Action, server),
    {QOS, Channel}.

-spec update_session(term(), session()) -> session().
update_session({ok, S1, PlayerInfo}, _Session) ->
    % Update player and the session info
    ID = ow_session:get_id(S1),
    update_player(PlayerInfo, ID),
    S1;
update_session({ok, S1}, _Session) ->
    % Update session info
    S1;
update_session(_, Session) ->
    % No change, return old session info
    Session.
