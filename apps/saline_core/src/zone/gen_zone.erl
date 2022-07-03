-module(gen_zone).
-behaviour(gen_server).

% gen_server
-export([start/3, start/4]).
-export([start_link/3, start_link/4]).
-export([start_monitor/3, start_monitor/4]).
-export([call/2, call/3]).
-export([cast/2]).
-export([reply/2]).
-export([stop/1, stop/3]).
% gen_zone specific calls

% must have corresponding callbacks
-export([
    join/3,
    join/2,
    part/3,
    part/2,
    rpc/4,
    who/1,
    status/1
]).

% helper functions
-export([is_player/2]).

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
-type session() :: saline_session:session().
-type session_id() :: integer().
-type player_list() :: [] | [player(), ...].
-type zone_msg() :: {atom(), map()}.
-type gen_zone_resp() ::
    noreply
    | {'@zone', zone_msg()}
    | {{'@', list()}, zone_msg()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG_I(Msg), {'$gen_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    players = [] :: player_list(),
    tick_timer :: undefined | reference(),
    tick_rate :: pos_integer(),
    require_auth :: boolean(),
    rpcs :: saline_rpcs:callbacks()
}).

-record(player, {
    id :: integer(),
    pid :: pid() | undefined,
    serializer :: saline_session:serializer()
}).
-type player() :: #player{}.

-define(DEFAULT_CONFIG, #{
    tick_rate => 30,
    require_auth => false
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_zone callbacks
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

-callback handle_join(Session, Players, State) -> Result when
    Session :: session(),
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Reply, State},
    Status :: ok | {ok, Session},
    Reply :: gen_zone_resp().
-optional_callbacks([handle_join/3]).

-callback handle_join(Msg, Session, Players, State) -> Result when
    Msg :: term(),
    Session :: session(),
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Reply, State},
    Status :: ok | {ok, Session},
    Reply :: gen_zone_resp().
-optional_callbacks([handle_join/4]).

-callback handle_part(Session, Players, State) -> Result when
    Session :: session(),
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok | {ok, Session},
    Response :: gen_zone_resp().
-optional_callbacks([handle_part/3]).

-callback handle_part(Msg, Session, Players, State) -> Result when
    Msg :: term(),
    Session :: session(),
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok | {ok, Session},
    Response :: gen_zone_resp().
-optional_callbacks([handle_part/4]).

-callback handle_rpc(Type, Msg, Session, Players, State) -> Result when
    Type :: atom(),
    Msg :: term(),
    Session :: session(),
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok | {ok, Session},
    Response :: gen_zone_resp().

-callback handle_tick(Players, State) -> Result when
    Players :: player_list(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: atom(),
    Response :: gen_zone_resp().

-callback rpc_info() -> Result when
    Result :: saline_rpc:callbacks().
-optional_callbacks([rpc_info/0]).

-callback handle_status(State) -> Result when
    State :: term(),
    Result :: term().
-optional_callbacks([handle_status/1]).

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
%%% public helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_player(session_id(), player_list()) -> boolean().
is_player(ID, PlayerList) ->
    lists:keymember(ID, #player.id, PlayerList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public behavior API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec join(server_ref(), term(), session()) -> {ok, session()}.
join(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({join, Msg, Session})).

-spec join(server_ref(), session()) -> {ok, session()}.
join(ServerRef, Session) ->
    gen_server:call(ServerRef, ?TAG_I({join, Session})).

-spec part(server_ref(), term(), session()) -> {ok, session()}.
part(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({part, Msg, Session})).

-spec part(server_ref(), session()) -> {ok, session()}.
part(ServerRef, Session) ->
    gen_server:call(ServerRef, ?TAG_I({part, Session})).

-spec rpc(server_ref(), atom(), term(), session()) -> {ok, session()}.
rpc(ServerRef, Type, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({rpc, Type, Msg, Session})).

-spec who(server_ref()) -> list().
who(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I({who})).

-spec status(server_ref()) -> undefined | term().
status(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I({status})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback functions for internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Initialize the internal state of the zone, with timer
init({CbMod, CbArgs}) ->
    init(CbMod, CbMod:init(CbArgs)).
init(CbMod, {ok, CbData, ConfigMap}) ->
    Config = maps:merge(?DEFAULT_CONFIG, ConfigMap),
    State = initialize_state(CbMod, CbData, Config),
    {ok, State};
init(CbMod, {ok, CbData}) ->
    Config = ?DEFAULT_CONFIG,
    State = initialize_state(CbMod, CbData, Config),
    {ok, State};
init(_CbMod, ignore) ->
    ignore;
init(_CbMod, Stop) ->
    Stop.

initialize_state(CbMod, CbData, Config) ->
    TickRate = maps:get(tick_rate, Config),
    RequireAuth = maps:get(require_auth, Config),
    Timer = erlang:send_after(1, self(), ?TAG_I(tick)),
    RPCInfo =
        case rpc_info(CbMod) of
            [] ->
                [];
            RPCs ->
                logger:debug("Registering ~p", [CbMod]),
                saline_protocol:register(CbMod),
                RPCs
        end,
    #state{
        cb_mod = CbMod,
        cb_data = CbData,
        tick_timer = Timer,
        tick_rate = TickRate,
        require_auth = RequireAuth,
        rpcs = RPCInfo
    }.

handle_call(?TAG_I({Action, Msg, Session}), _From, St0) ->
    % where Action = join or part.
    {Session1, St1} = maybe_auth_do(Action, Msg, Session, St0),
    {reply, {ok, Session1}, St1};
handle_call(?TAG_I({Action, Session}), _From, St0) ->
    % where Action = join or part.
    {Session1, St1} = maybe_auth_do(Action, Session, St0),
    {reply, {ok, Session1}, St1};
handle_call(?TAG_I({who}), _From, St0) ->
    Players = St0#state.players,
    IDs = [X#player.id || X <- Players],
    {reply, IDs, St0};
handle_call(?TAG_I({status}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    {StatusMsg, CbData1} =
        case erlang:function_exported(CbMod, handle_status, 1) of
            true ->
                CbMod:handle_status(CbData);
            _ ->
                {undefined, CbData}
        end,
    St1 = St0#state{cb_data = CbData1},
    {reply, StatusMsg, St1};
handle_call(?TAG_I({rpc, Type, Msg, Session}), _From, St0) ->
    {Session1, St1} = maybe_auth_rpc(Type, Msg, Session, St0),
    {reply, {ok, Session1}, St1};
handle_call(_Call, _From, St0) ->
    {reply, ok, St0}.

handle_cast(_Cast, St0) ->
    {noreply, St0}.

handle_info(?TAG_I(tick), St0) ->
    TimerRef = St0#state.tick_timer,
    erlang:cancel_timer(TimerRef),
    TickRate = St0#state.tick_rate,
    T1 = erlang:send_after(TickRate, self(), ?TAG_I(tick)),
    St1 = tick(St0),
    {noreply, St1#state{tick_timer = T1}}.

terminate(_Reason, _St0) -> ok.
code_change(_OldVsn, St0, _Extra) -> {ok, St0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0, players = Players}) ->
    PlayerIDs = [X#player.id || X <- Players],
    {_Status, Notify, CbData1} = CbMod:handle_tick(PlayerIDs, CbData0),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    St1.

rpc_info(CbMod) ->
    case erlang:function_exported(CbMod, rpc_info, 0) of
        true ->
            CbMod:rpc_info();
        _ ->
            logger:debug("~p:rpc_info/0 not exported ignoring", [CbMod]),
            []
    end.

handle_notify({{'@', _}, _}, #state{players = []}) ->
    % NO MESSAGE: The last player has left, nobody left to notify but don't
    %             crash.
    ok;
handle_notify({{'@', IDs}, {MsgType, Msg}}, #state{
    players = Players, rpcs = RPCs
}) ->
    % SEND MESSAGE: Filter just for the players we want to notify
    P = [lists:keyfind(ID, #player.id, Players) || ID <- IDs],
    notify_players(MsgType, Msg, RPCs, P);
handle_notify({'@zone', _}, #state{players = []}) ->
    % NO MESSAGE: There are messages to send, but no one left to receive them,
    %             so do nothing.
    ok;
handle_notify({'@zone', {MsgType, Msg}}, #state{players = Players, rpcs = RPCs}) ->
    % SEND MESSAGE: Send everyone the message
    notify_players(MsgType, Msg, RPCs, Players);
handle_notify(noreply, _State) ->
    % NO MESSAGE
    ok.

player_add(Session, St0) ->
    % Add the player to the list of players.
    % TODO: This may end up inefficient later, if we filter the list every
    % time we want to send messages depending on the serializer.
    % Profile it and readjust gen_zone as needed.
    ID = saline_session:get_id(Session),
    PID = saline_session:get_pid(Session),
    Serializer = saline_session:get_serializer(Session),
    Players = St0#state.players,
    Player = #player{id = ID, pid = PID, serializer = Serializer},
    St0#state{players = lists:keystore(ID, #player.id, Players, Player)}.

player_rm(Session, St0) ->
    Players0 = St0#state.players,
    ID = saline_session:get_id(Session),
    Players1 = lists:keydelete(ID, #player.id, Players0),
    St0#state{players = Players1}.

notify_players(MsgType, Msg, RPCs, Players) ->
    Send = fun(Player) ->
        % causes a crash if the pid doesn't exist
        case Player#player.pid of
            undefined ->
                ok;
            Pid ->
                case Player#player.serializer of
                    undefined ->
                        Pid ! {self(), zone_msg, {MsgType, Msg}};
                    protobuf ->
                        RPC = saline_rpc:find_call(MsgType, RPCs),
                        EMod = saline_rpc:encoder(RPC),
                        OpCode = saline_rpc:opcode(RPC),
                        EncodedMsg = EMod:encode_msg(Msg, MsgType),
                        Pid ! {self, zone_msg, [<<OpCode:16>>, EncodedMsg]}
                end
        end
    end,
    lists:foreach(Send, Players).

decode_msg(MsgType, Msg, RPCs, Session) ->
    Serializer = saline_session:get_serializer(Session),
    case Serializer of
        undefined ->
            Msg;
        protobuf ->
            RPC = saline_rpc:find_handler(MsgType, RPCs),
            EMod = saline_rpc:encoder(RPC),
            EMod:decode_msg(Msg, MsgType)
    end.

maybe_auth_do(Action, Msg, Session, St0 = #state{require_auth = RA}) when
    RA =:= true
->
    case saline_session:is_authenticated(Session) of
        true ->
            actually_do(Action, Msg, Session, St0);
        false ->
            % Do not update session, do not update state.
            {Session, St0}
    end;
maybe_auth_do(Action, Msg, Session, St0) ->
    actually_do(Action, Msg, Session, St0).
maybe_auth_do(Action, Session, St0 = #state{require_auth = RA}) when
    RA =:= true
->
    case saline_session:is_authenticated(Session) of
        true ->
            actually_do(Action, Session, St0);
        false ->
            % Do not update session, do not update state.
            {Session, St0}
    end;
maybe_auth_do(Action, Session, St0) ->
    actually_do(Action, Session, St0).

actually_do(Action, Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    Players = St0#state.players,
    CbFun = list_to_existing_atom("handle_" ++ atom_to_list(Action)),
    {Status, Notify, CbData1} = CbMod:CbFun(
        Session, Players, CbData0
    ),
    case Action of
        join ->
            add_and_notify(Session, St0, Status, CbMod, CbData1, Notify);
        part ->
            rm_and_notify(Session, St0, Status, CbData1, Notify)
    end.
actually_do(Action, Msg, Session, St0) ->
    RPCs = St0#state.rpcs,
    DecodedMsg = decode_msg(Action, Msg, RPCs, Session),
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    Players = St0#state.players,
    CbFun = list_to_existing_atom("handle_" ++ atom_to_list(Action)),
    {Status, Notify, CbData1} = CbMod:CbFun(
        DecodedMsg, Session, Players, CbData0
    ),
    case Action of
        join ->
            add_and_notify(Session, St0, Status, CbMod, CbData1, Notify);
        part ->
            rm_and_notify(Session, St0, Status, CbData1, Notify)
    end.

add_and_notify(Session0, St0, Status, CbMod, CbData1, Notify) ->
    {Session1, St1} =
        case Status of
            ok ->
                % No session update by this server, continue with existing one
                {Session0, player_add(Session0, St0)};
            {ok, S1} ->
                {S1, player_add(S1, St0)};
            _ ->
                {Session0, St0}
        end,
    St2 = St1#state{cb_data = CbData1},
    handle_notify(Notify, St2),
    % Set the player's termination callback
    Session2 = saline_session:set_termination_callback(
        {CbMod, part, 1}, Session1
    ),
    {Session2, St2}.

rm_and_notify(Session0, St0, Status, CbData1, Notify) ->
    {Session1, St1} =
        case Status of
            ok ->
                % Remove the player from our list
                {Session0, player_rm(Session0, St0)};
            {ok, S1} ->
                {S1, player_rm(S1, St0)};
            _ ->
                {Session0, St0}
        end,
    % Send any messages as needed - called for side effects
    St2 = St1#state{cb_data = CbData1},
    handle_notify(Notify, St2),
    {Session1, St2}.

maybe_auth_rpc(Type, Msg, Session, St0 = #state{require_auth = RA}) when
    RA =:= true
->
    case saline_session:is_authenticated(Session) of
        true ->
            actually_rpc(Type, Msg, Session, St0);
        false ->
            {Session, St0}
    end;
maybe_auth_rpc(Type, Msg, Session, St0) ->
    actually_rpc(Type, Msg, Session, St0).

actually_rpc(Type, Msg, Session, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    RPCs = St0#state.rpcs,
    Players = St0#state.players,
    DecodedMsg = decode_msg(Type, Msg, RPCs, Session),
    {Status, Notify, CbData1} = CbMod:handle_rpc(
        Type, DecodedMsg, Session, Players, CbData
    ),
    Session1 =
        case Status of
            ok -> Session;
            {ok, S1} -> S1
        end,
    % Send any messages as needed - called for side effects
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    {Session1, St1}.
