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
    {ok, term()}
    | {{'@zone', term()}, term()}
    | {{'@', list(), term()}, term()}.

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
        | ignore
        | {stop, Reason},
    InitialData :: term(),
    Reason :: term().

-callback handle_join(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_part(Session, State) -> Result when
    Session :: session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_action(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_tick(State) -> Result when
    State :: term(),
    Result :: term().

-callback handle_state_xfer(State) -> Result when
    State :: term(),
    Result :: term().

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

-spec join(server_ref(), term(), session()) -> gen_zone_resp().
join(ServerRef, Msg, Session) ->
    % Check that the session is valid before doing anything
    case saline_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({join, Msg, Session}));
        false ->
            {ok, Session}
    end.

-spec part(server_ref(), session()) -> gen_zone_resp().
part(ServerRef, Session) ->
    % Check that the session is valid before doing anything
    case saline_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({part, Session}));
        false ->
            ok
    end.

-spec action(server_ref(), term(), session()) -> gen_zone_resp().
action(ServerRef, Msg, Session) ->
    case saline_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({action, Msg, Session}));
        false ->
            ok
    end.

-spec who(server_ref()) -> list().
who(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I({who})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback functions for internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Initialize the internal state of the zone, with timer
init({CbMod, CbArgs}) ->
    case CbMod:init(CbArgs) of
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
    {Status, Notify, Session1, CbData1} = CbMod:handle_join(
        Msg, Session, CbData0
    ),
    St1 =
        case Status of
            ok ->
                % Add the player to our list
                add_player(Session, St0);
            _ ->
                St0
        end,
    % Send any messages as needed - called for side effects
    St2 = St1#state{cb_data = CbData1},
    handle_notify(join, Notify, St2),
    {reply, {ok, Session1}, St2};
handle_call(?TAG_I({part, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData0 = St0#state.cb_data,
    {Status, Notify, Session1, CbData1} = CbMod:handle_part(Session, CbData0),
    St1 =
        case Status of
            ok ->
                % Add the player to our list
                rm_player(Session, St0);
            _ ->
                St0
        end,
    % Send any messages as needed - called for side effects
    St2 = St1#state{cb_data = CbData1},
    handle_notify(part, Notify, St2),
    {reply, {ok, Session1}, St2};
handle_call(?TAG_I({action, Msg, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    {_, Notify, Session1, CbData1} = CbMod:handle_action(Msg, Session, CbData),
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

tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0}) ->
    case CbMod:handle_tick(CbData0) of
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

handle_notify(MsgType, {'@', IDs, Msg}, #state{players = Players, rpcs = RPCs}) when
    is_list(IDs) and Players =/= []
->
    % Filter just for the players we want to notify
    P = [lists:keyfind(ID, #player.id, Players) || ID <- IDs],
    notify_players(MsgType, Msg, RPCs, P);
handle_notify(MsgType, {'@', ID, Msg}, #state{players = Players, rpcs = RPCs}) when
    Players =/= []
->
    P = lists:keyfind(ID, #player.id, Players),
    notify_players(MsgType, Msg, RPCs, [P]);
handle_notify(_MT, {'@', _, _}, #state{players = Players}) when
    Players =:= []
->
    % The last player has left, nobody left to notify but don't crash.
    ok;
handle_notify(MsgType, {'@zone', Msg}, #state{players = Players, rpcs = RPCs}) when
    Players =/= []
->
    notify_players(MsgType, Msg, RPCs, Players);
handle_notify(_MT, {'@zone', _Msg}, #state{players = Players}) when
    Players =:= []
->
    % There are messages to send, but no one left to receive them, so do nothing
    % rather than crash.
    ok.

add_player(Session, St0) ->
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

rm_player(Session, St0) ->
    Players0 = St0#state.players,
    ID = saline_session:get_id(Session),
    Players1 = lists:keydelete(ID, #player.id, Players0),
    St0#state{players = Players1}.

notify_players(MsgType, Msg, RPCs, Players) ->
    Send = fun(Player) ->
        Pid = Player#player.pid,
        case Player#player.serializer of
            none ->
                Pid ! {self(), '@zone', {MsgType, Msg}};
            protobuf ->
                RPC = saline_rpc:find_call(MsgType, RPCs),
                EMod = saline_rpc:encoder(RPC),
                OpCode = saline_rpc:opcode(RPC),
                EncodedMsg = EMod:encode_msg(Msg),
                Pid ! {self, zone_msg, [OpCode, EncodedMsg]}
        end
    end,
    lists:foreach(Send, Players).
