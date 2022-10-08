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
% ow_zone specific calls

% must have corresponding callbacks
-export([
    join/3,
    join/2,
    part/3,
    part/2,
    rpc/4,
    who/1,
    status/1,
    broadcast/2
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG_I(Msg), {'$ow_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    tick_rate :: pos_integer(),
    require_auth :: boolean(),
    rpcs :: ow_rpcs:callbacks()
}).

-define(DEFAULT_CONFIG, #{
    % ticks/sec
    tick_rate => 40,
    require_auth => false
}).

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

-callback handle_join(Session, State) -> Result when
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().
-optional_callbacks([handle_join/2]).

-callback handle_join(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().
-optional_callbacks([handle_join/3]).

-callback handle_part(Session, State) -> Result when
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().
-optional_callbacks([handle_part/2]).

-callback handle_part(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().
-optional_callbacks([handle_part/3]).

-callback handle_rpc(Type, Msg, Session, State) -> Result when
    Type :: atom(),
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Response, Status, State},
    Status :: atom() | {ok, Session},
    Response :: ow_zone_resp().

-callback handle_tick(TickRate, State) -> Result when
    TickRate :: pos_integer(),
    State :: term(),
    Result :: {Response, State},
    Response :: ow_zone_resp().

-callback rpc_info() -> Result when
    Result :: ow_rpc:callbacks().
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

-spec broadcast(server_ref(), term()) -> ok.
broadcast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, ?TAG_I({broadcast, Msg})).

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

initialize_state(CbMod, CbData, Config = #{tick_rate := TickRate}) ->
    % setup the timer
    timer:send_interval(TickRate, self(), ?TAG_I(tick)),
    % configure auth
    RequireAuth = maps:get(require_auth, Config),
    RPCInfo =
        case rpc_info(CbMod) of
            [] ->
                [];
            RPCs ->
                logger:debug("Registering ~p", [CbMod]),
                ow_protocol:register(CbMod),
                RPCs
        end,
    #state{
        cb_mod = CbMod,
        cb_data = CbData,
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
    Players = ow_player_reg:list(self()),
    IDs = [ow_player_reg:get_id(P) || P <- Players],
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

handle_cast(?TAG_I({broadcast, Msg}), St0) ->
    io:format("Sending broadcast..~n"),
    handle_notify({'@zone', Msg}, St0),
    {noreply, St0};
handle_cast(_Cast, St0) ->
    {noreply, St0}.

handle_info(?TAG_I(tick), St0) ->
    St1 = tick(St0),
    {noreply, St1}.

terminate(_Reason, _St0) -> ok.
code_change(_OldVsn, St0, _Extra) -> {ok, St0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0}) ->
    Config = #{tick_rate => St0#state.tick_rate},
    {Notify, CbData1} = CbMod:handle_tick(Config, CbData0),
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    St1.

rpc_info(CbMod) ->
    case erlang:function_exported(CbMod, rpc_info, 0) of
        true ->
            CbMod:rpc_info();
        _ ->
            []
    end.

handle_notify({{'@', IDs}, {MsgType, Msg}}, #state{rpcs = RPCs}) ->
    % SEND MESSAGE: Filter just for the players we want to notify
    Players = [ow_player_reg:get(ID) || ID <- IDs],
    case Players of
        [] ->
            % Nothing to send
            ok;
        Players ->
            notify_players(MsgType, Msg, RPCs, Players)
    end;
handle_notify({'@zone', {MsgType, Msg}}, #state{rpcs = RPCs}) ->
    % SEND MESSAGE: Send everyone the message
    Players = ow_player_reg:list(self()),
    case Players of
        [] ->
            % Nothing to send
            ok;
        Players ->
            notify_players(MsgType, Msg, RPCs, Players)
    end;
handle_notify(noreply, _State) ->
    % NO MESSAGE
    ok.

player_add(PlayerInfo, Session) ->
    % Add the player to the list of players.
    ID = ow_session:get_id(Session),
    PID = ow_session:get_pid(Session),
    Serializer = ow_session:get_serializer(Session),
    % Force a crash via failed match if player registration doesn't go well
    case ow_player_reg:new(ID, PID, Serializer, self(), PlayerInfo) of
        ok -> ok
    end.

player_rm(Session) ->
    ID = ow_session:get_id(Session),
    ow_player_reg:delete(ID).

notify_players(MsgType, Msg, RPCs, Players) ->
    Send = fun(Player) ->
        % causes a crash if the pid doesn't exist
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
                        RPC = ow_rpc:find_call(MsgType, RPCs),
                        EMod = ow_rpc:encoder(RPC),
                        OpCode = ow_rpc:opcode(RPC),
                        EncodedMsg = EMod:encode_msg(Msg, MsgType),
                        % TODO: to which process is self() referring?
                        Pid !
                            {self(), zone_msg, [<<OpCode:16>>, EncodedMsg]}
                end
        end
    end,
    lists:foreach(Send, Players).

decode_msg(_MsgType, <<>>, _RPCs, _Session) ->
    % Suppose you have a message that is just a repeated type.
    % If the number of entries is 0, you just get a <<>> back after stripping
    % off the opcode. So we return an empty list here.
    [];
decode_msg(MsgType, Msg, RPCs, Session) ->
    Serializer = ow_session:get_serializer(Session),
    case Serializer of
        undefined ->
            Msg;
        protobuf ->
            RPC = ow_rpc:find_handler(MsgType, RPCs),
            EMod = ow_rpc:encoder(RPC),
            EMod:decode_msg(Msg, MsgType)
    end.

maybe_auth_do(Action, Msg, Session, St0 = #state{require_auth = RA}) when
    RA =:= true
->
    case ow_session:is_authenticated(Session) of
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
    case ow_session:is_authenticated(Session) of
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
    CbFun = list_to_existing_atom("handle_" ++ atom_to_list(Action)),
    {Notify, Status, CbData1} = CbMod:CbFun(Session, CbData0),
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
    CbFun = list_to_existing_atom("handle_" ++ atom_to_list(Action)),
    {Notify, Status, CbData1} = CbMod:CbFun(DecodedMsg, Session, CbData0),
    case Action of
        join ->
            add_and_notify(Session, St0, Status, CbMod, CbData1, Notify);
        part ->
            rm_and_notify(Session, St0, Status, CbData1, Notify)
    end.

add_and_notify(Session0, St0, Status, CbMod, CbData1, Notify) ->
    Session1 =
        case Status of
            {ok, S1, PlayerInfo} ->
                player_add(PlayerInfo, S1),
                S1;
            {ok, S1} ->
                player_add(undefined, S1),
                S1;
            _ ->
                Session0
        end,
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    % Set the player's termination callback
    Session2 = ow_session:set_termination_callback(
        {CbMod, part, 1}, Session1
    ),
    {Session2, St1}.

rm_and_notify(Session0, St0, Status, CbData1, Notify) ->
    % TODO: This needs some significant thought. In a single zone case - it
    % makes sense to just delete the user from the player registry. In the
    % multi-zone case this may not make sense.
    % I think this code needs to be a bit more flexible.
    Session1 =
        case Status of
            {ok, S1, PlayerInfo} ->
                ID = ow_session:get_id(S1),
                update_player(PlayerInfo, ID),
                S1;
            {ok, S1} ->
                S1;
            _ ->
                Session0
        end,
    player_rm(Session1),
    % Send any messages as needed - called for side effects
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    {Session1, St1}.

maybe_auth_rpc(Type, Msg, Session, St0 = #state{require_auth = RA}) when
    RA =:= true
->
    case ow_session:is_authenticated(Session) of
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
    DecodedMsg = decode_msg(Type, Msg, RPCs, Session),
    SessionID = ow_session:get_id(Session),
    % Overworld will confirm that the player is actually part of the zone to
    % which they are sending RPCs
    {Notify, Status, CbData1} =
        case is_player(SessionID) of
            true ->
                CbMod:handle_rpc(Type, DecodedMsg, Session, CbData);
            false ->
                {noreply, ok, CbData}
        end,
    Session1 =
        case Status of
            {ok, S1, PlayerInfo} ->
                ID = ow_session:get_id(S1),
                update_player(PlayerInfo, ID),
                S1;
            {ok, S1} ->
                S1;
            _ ->
                Session
        end,
    % Send any messages as needed - called for side effects
    St1 = St0#state{cb_data = CbData1},
    handle_notify(Notify, St1),
    {Session1, St1}.

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
