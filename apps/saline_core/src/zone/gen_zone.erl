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
-export([join/3, part/2, action/3, who/1]).

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
-type server_ref() :: gen_server:server_ref().
-type session() :: saline_session:session().
-type gen_zone_resp() ::
    noreply
    | {'@zone', term()}
    | {'@', list(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG_I(Msg), {'$gen_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    players = [] :: list(),
    tick_timer :: undefined | reference(),
    tick_rate :: pos_integer(),
    rpcs :: saline_rpcs:callbacks()
}).

-record(player, {
    id :: integer(),
    pid :: pid(),
    serializer :: saline_session:serializer()
}).

-define(TICK_RATE, 30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_zone callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback init(Args) -> Result when
    Args :: term(),
    Result ::
        {ok, InitialData}
        | {ok, InitialData, TickRate}
        | ignore
        | {stop, Reason},
    TickRate :: pos_integer(),
    InitialData :: term(),
    Reason :: term().

-callback handle_join(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Status, Reply, State},
    Status :: ok | {ok, Session},
    Reply :: gen_zone_resp().

-callback handle_part(Session, State) -> Result when
    Session :: session(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok | {ok, Session},
    Response :: gen_zone_resp().

-callback handle_action(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok | {ok, Session},
    Response :: gen_zone_resp().

-callback handle_tick(Players, State) -> Result when
    Players :: list(),
    State :: term(),
    Result :: {Status, Response, State},
    Status :: ok,
    Response :: gen_zone_resp().

% not sure we need this guy.
-callback handle_state_xfer(State) -> Result when
    State :: term(),
    Result :: term().
-optional_callbacks([handle_state_xfer/1]).

-callback rpc_info() -> Result when
    Result :: saline_rpc:callbacks().
-optional_callbacks([rpc_info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Module, Args, Opts) ->
    gen_server:start(?MODULE, {Module, Args}, Opts).

start(ServerName, Module, Args, Opts) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Opts).

start_link(Module, Args, Opts) ->
    gen_server:start_link(?MODULE, {Module, Args}, Opts).

start_link(ServerName, Module, Args, Opts) ->
    % TODO: Check for configuration options in Opts
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Opts).

start_monitor(Module, Args, Opts) ->
    gen_server:start_monitor(?MODULE, {Module, Args}, Opts).

start_monitor(ServerName, Module, Args, Opts) ->
    gen_server:start_monitor(ServerName, ?MODULE, {Module, Args}, Opts).

call(ServerRef, Msg) ->
    gen_server:call(ServerRef, Msg).

call(ServerRef, Msg, Timeout) ->
    gen_server:call(ServerRef, Msg, Timeout).

cast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, Msg).

reply(From, Reply) ->
    gen_server:reply(From, Reply).

stop(ServerRef) ->
    gen_server:stop(ServerRef).

stop(ServerRef, Reason, Timeout) ->
    gen_server:stop(ServerRef, Reason, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public behavior API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec join(server_ref(), term(), session()) -> {ok, session()}.
join(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({join, Msg, Session})).

-spec part(server_ref(), session()) -> {ok, session()}.
part(ServerRef, Session) ->
    gen_server:call(ServerRef, ?TAG_I({part, Session})).

-spec action(server_ref(), term(), session()) -> {ok, session()}.
action(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({action, Msg, Session})).

-spec who(server_ref()) -> list().
who(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I({who})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback functions for internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Initialize the internal state of the zone, with timer
init({CbMod, CbArgs}) ->
    case CbMod:init(CbArgs) of
        {ok, CbData, TickRate} ->
            Timer = erlang:send_after(1, self(), ?TAG_I(tick)),
            {ok, #state{
                cb_mod = CbMod,
                cb_data = CbData,
                tick_timer = Timer,
                tick_rate = TickRate,
                rpcs = rpc_info(CbMod)
            }};
        {ok, CbData} ->
            Timer = erlang:send_after(1, self(), ?TAG_I(tick)),
            {ok, #state{
                cb_mod = CbMod,
                cb_data = CbData,
                tick_timer = Timer,
                tick_rate = ?TICK_RATE,
                rpcs = rpc_info(CbMod)
            }};
        ignore ->
            ignore;
        Stop = {stop, _Reason} ->
            Stop
    end.

handle_call(?TAG_I({join, Msg, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    RPCs = St0#state.rpcs,
    Msg1 = decode_msg(join, Msg, RPCs, Session),
    {Status, Notify, CbData1} = CbMod:handle_join(Msg1, Session, CbData0),
    {Session1, St1} =
        case Status of
            ok ->
                % Server didn't update the session, just send along the one we have
                {Session, player_add(Session, St0)};
            {ok, S1} ->
                {S1, player_add(S1, St0)};
            _ ->
                {Session, St0}
        end,
    % Send any messages as needed - called for side effects
    St2 = St1#state{cb_data = CbData1},
    handle_notify(join, Notify, St2),
    {reply, {ok, Session1}, St2};
handle_call(?TAG_I({part, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    {Status, Notify, CbData1} = CbMod:handle_part(Session, CbData0),
    {Session1, St1} =
        case Status of
            ok ->
                % Remove the player from our list
                {Session, player_rm(Session, St0)};
            {ok, S1} ->
                {S1, player_rm(S1, St0)}
        end,
    % Send any messages as needed - called for side effects
    St2 = St1#state{cb_data = CbData1},
    handle_notify(part, Notify, St2),
    {reply, {ok, Session1}, St2};
handle_call(?TAG_I({action, Msg, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    RPCs = St0#state.rpcs,
    Msg1 = decode_msg(action, Msg, Session, RPCs),
    % We don't take any actions after an action because it won't update any
    % internal state of gen_zone. Just accept the new state data and any
    % updates to the player's session from the callback module.
    {Status, Notify, CbData1} = CbMod:handle_action(Msg1, Session, CbData),
    Session1 =
        case Status of
            ok -> Session;
            {ok, S1} -> S1
        end,
    % Send any messages as needed - called for side effects
    St1 = St0#state{cb_data = CbData1},
    handle_notify(action, Notify, St1),
    {reply, {ok, Session1}, St1};
handle_call(?TAG_I({who}), _From, St0) ->
    Players = St0#state.players,
    IDs = [X#player.id || X <- Players],
    {reply, IDs, St0};
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
    case CbMod:handle_tick(PlayerIDs, CbData0) of
        {ok, Notify, CbData1} ->
            St1 = St0#state{cb_data = CbData1},
            handle_notify(tick, Notify, St1),
            St1
    end.

rpc_info(CbMod) ->
    case erlang:function_exported(CbMod, rpc_info, 0) of
        true ->
            CbMod:rpc_info();
        _ ->
            []
    end.

handle_notify(_MT, {'@', _, _}, #state{players = []}) ->
    % The last player has left, nobody left to notify but don't crash.
    ok;
handle_notify(MsgType, {'@', IDs, Msg}, #state{players = Players, rpcs = RPCs}) ->
    % Filter just for the players we want to notify
    P = [lists:keyfind(ID, #player.id, Players) || ID <- IDs],
    notify_players(MsgType, Msg, RPCs, P);
handle_notify(_MT, {'@zone', _Msg}, #state{players = []}) ->
    % There are messages to send, but no one left to receive them, so do nothing.
    ok;
handle_notify(MsgType, {'@zone', Msg}, #state{players = Players, rpcs = RPCs}) ->
    notify_players(MsgType, Msg, RPCs, Players);
handle_notify(_MT, noreply, _State) ->
    % No reply
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
        Pid = Player#player.pid,
        case Player#player.serializer of
            undefined ->
                Pid ! {self(), '@zone', {MsgType, Msg}};
            protobuf ->
                RPC = saline_rpc:find_call(MsgType, RPCs),
                EMod = saline_rpc:encoder(RPC),
                OpCode = saline_rpc:opcode(RPC),
                EncodedMsg = EMod:encode_msg(Msg, MsgType),
                Pid ! {self, zone_msg, [OpCode, EncodedMsg]}
        end
    end,
    lists:foreach(Send, Players).

decode_msg(MsgType, Msg, RPCs, Session) ->
    Serializer = saline_session:get_serializer(Session),
    case Serializer of
        undefined ->
            Msg;
        protobuf ->
            RPC = saline_rpc:find_call(MsgType, RPCs),
            EMod = saline_rpc:encoder(RPC),
            EMod:decode_msg(Msg, MsgType)
    end.
