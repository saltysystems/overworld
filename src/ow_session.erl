%%=========================================================================
%% Overworld Session
%%
%% This module defines and holds a session's state
%%
%%=========================================================================
-module(ow_session).
-behaviour(gen_statem).

% API
-export([
    start/1, start/0,
    stop/1,
    id/2, id/1,
    connect/1,
    disconnect/1,
    proxy/2, proxy/1,
    serializer/2, serializer/1,
    latency/2, latency/1,
    game_data/2, game_data/1,
    disconnect_callback/2, disconnect_callback/1,
    token/2, token/1,
    zone/2, zone/1
]).

% gen_statem required callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3
]).
% state functions
-export([
    disconnected/3,
    connected/3,
    active/3
]).

-type serializer() :: undefined | protobuf.
-type id() :: pos_integer().
-type token() :: binary() | undefined.
-type time_ms() :: non_neg_integer().
-type mfargs() :: {atom(), atom(), list()}.
-type proxy_pid() :: pid() | undefined.
-type zone_pid() :: pid() | undefined.

-record(session, {
    % internal use only
    id :: id(),
    proxy :: proxy_pid(),
    serializer :: serializer(),
    latency :: time_ms(),
    game_data :: any(),
    disconnect_callback :: mfargs() | undefined,
    disconnect_timeout :: time_ms(),
    shutdown_delay :: time_ms(),
    token :: token(),
    zone :: zone_pid()
}).
-export_type([serializer/0]).
-export_type([id/0]).

-define(DEFAULT_DISCONNECT_TIMEOUT, 5000).
% A bit hackish, but gives the zone some time to deal with shutdowns
-define(DEFAULT_SHUTDOWN_DELAY, 1000).

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Start the session server and create a new session with this ID
%% @end
%%----------------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    start([]).
-spec start([tuple()]) -> {ok, pid()}.
start(Config) ->
    gen_statem:start(?MODULE, [Config], []).

%%----------------------------------------------------------------------------
%% @doc Stop the session server
%% @end
%%----------------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%----------------------------------------------------------------------------
%% @doc Set the session ID for this session (network serializable)
%% @end
%%----------------------------------------------------------------------------
-spec id(pid(), id()) -> {ok, id()}.
id(PID, SessionID) ->
    gen_statem:call(PID, {set_id, SessionID}).

%%----------------------------------------------------------------------------
%% @doc Get the session id of this session (network serializable)
%% @end
%%----------------------------------------------------------------------------
-spec id(pid()) -> id() | undefined.
id(PID) ->
    gen_statem:call(PID, get_id).

%%----------------------------------------------------------------------------
%% @doc Alias for proxy(PID, self())
%% @end
%%----------------------------------------------------------------------------
-spec connect(pid()) -> {ok, proxy_pid()}.
connect(PID) ->
    proxy(PID, self()).

%%----------------------------------------------------------------------------
%% @doc Alias for proxy(PID, undefined)
%% @end
%%----------------------------------------------------------------------------
-spec disconnect(pid()) -> {ok, proxy_pid()}.
disconnect(PID) ->
    proxy(PID, undefined).

%%----------------------------------------------------------------------------
%% @doc Set the pid of the session's proxy process, including ENet, WebSocket
%%      and internal handlers.
%% @end
%%----------------------------------------------------------------------------
-spec proxy(pid(), proxy_pid()) -> {ok, proxy_pid()}.
proxy(PID, ProxyPID) ->
    gen_statem:call(PID, {set_proxy, ProxyPID}).

%%----------------------------------------------------------------------------
%% @doc Get the pid of the session's proxy process.
%% @end
%%----------------------------------------------------------------------------
-spec proxy(pid()) -> proxy_pid().
proxy(PID) ->
    gen_statem:call(PID, get_proxy).

%%----------------------------------------------------------------------------
%% @doc Set the format for serializing data. If communication happens all
%%      within Erlang node(s), then there is no need to set a serializer.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(serializer(), pid()) -> {ok, serializer()}.
serializer(Serializer, PID) ->
    gen_statem:call(PID, {set_serializer, Serializer}).

%%----------------------------------------------------------------------------
%% @doc Get the format for serializing data.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(pid()) -> serializer() | undefined.
serializer(PID) ->
    gen_statem:call(PID, get_serializer).

%%----------------------------------------------------------------------------
%% @doc Set the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(pos_integer(), pid()) -> {ok, pos_integer()}.
latency(Latency, PID) ->
    gen_statem:call(PID, {set_latency, Latency}).

%%----------------------------------------------------------------------------
%% @doc Get the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(pid()) -> non_neg_integer().
latency(PID) ->
    gen_statem:call(PID, get_latency).

%%----------------------------------------------------------------------------
%% @doc Set the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(any(), pid()) -> {ok, any()}.
game_data(Data, PID) ->
    gen_statem:call(PID, {set_game_data, Data}).

%%----------------------------------------------------------------------------
%% @doc Get the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(pid()) -> any().
game_data(PID) ->
    gen_statem:call(PID, get_game_info).

%%----------------------------------------------------------------------------
%% @doc Set the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec disconnect_callback(mfargs(), pid()) -> {ok, mfargs()}.
disconnect_callback(Callback, PID) ->
    gen_statem:call(PID, {set_disconnect_callback, Callback}).

%%----------------------------------------------------------------------------
%% @doc Get the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec disconnect_callback(pid()) -> undefined | mfargs().
disconnect_callback(PID) ->
    gen_statem:call(PID, get_disconnect_callback).

%%----------------------------------------------------------------------------
%% @doc Sets the session token
%% @end
%%----------------------------------------------------------------------------
-spec token(token(), pid()) -> {ok, token()}.
token(Token, PID) ->
    gen_statem:call(PID, {set_token, Token}).

%%----------------------------------------------------------------------------
%% @doc Get the session token.
%% @end
%%----------------------------------------------------------------------------
-spec token(pid()) -> token().
token(PID) ->
    gen_statem:call(PID, get_token).

%%----------------------------------------------------------------------------
%% @doc Sets the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(zone_pid(), pid()) -> {ok, pid()}.
zone(ZonePID, PID) ->
    gen_statem:call(PID, {set_zone, ZonePID}).

%%----------------------------------------------------------------------------
%% @doc Get the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(pid()) -> zone_pid().
zone(PID) ->
    gen_statem:call(PID, get_zone).

%%=========================================================================
%% Callback handlers
%%=========================================================================

init([Config]) ->
    Session = #session{
        id = proplists:get_value(id, Config, erlang:unique_integer([positive])),
        proxy = proplists:get_value(proxy, Config, undefined),
        serializer = proplists:get_value(serializer, Config, undefined),
        latency = proplists:get_value(latency, Config, 0),
        game_data = proplists:get_value(game_data, Config, undefined),
        disconnect_callback = proplists:get_value(
            disconnect_callback, Config, undefined
        ),
        disconnect_timeout = proplists:get_value(
            disconnect_timeout, Config, ?DEFAULT_DISCONNECT_TIMEOUT
        ),
        shutdown_delay = proplists:get_value(
            shutdown_delay, Config, ?DEFAULT_SHUTDOWN_DELAY
        ),
        token = proplists:get_value(token, Config, undefined),
        zone = undefined
    },
    % Register with gproc
    true = gproc:reg({n, l, Session#session.id}, ignored),
    % Set a timer to maybe terminate a stale session if it hasn't gone active
    % within the default timeout period
    case Session#session.proxy of
        undefined ->
            Timeout = Session#session.disconnect_timeout,
            erlang:send_after(Timeout, self(), maybe_terminate),
            {ok, disconnected, Session};
        _ ->
            {ok, connected, Session}
    end.

callback_mode() -> state_functions.

disconnected(info, maybe_terminate, _Session) ->
    logger:debug("Caught terminate in disconnected state. Stopping ~p.", [self()]),
    {stop, normal};
disconnected({call, From}, {set_proxy, ProxyPID}, Session) ->
    logger:debug("~p connecting to proxy ~p", [self(), ProxyPID]),
    Reply = {reply, From, {ok, ProxyPID}},
    {next_state, connected, Session#session{proxy = ProxyPID}, [Reply]};
disconnected({call, From}, {set_zone, ZonePID}, Session) ->
    logger:debug("Cannot connect to zone ~p: disconnected", [ZonePID]),
    Reply = {reply, From, {error, disconnected}},
    {keep_state, Session, [Reply]};
disconnected(EventType, EventContent, Session) ->
    handle_event(EventType, EventContent, Session).

connected({call, From}, {set_zone, ZonePID}, Session) ->
    logger:debug("~p requests zone be set to ~p", [From, ZonePID]),
    Reply = {reply, From, {ok, ZonePID}},
    {next_state, active, Session#session{zone = ZonePID}, [Reply]};
connected(EventType, EventContent, Session) ->
    handle_event(EventType, EventContent, Session).

active({call, From}, {set_zone, ZonePID}, Session) ->
    logger:debug("~p requests zone be set to ~p", [From, ZonePID]),
    Reply = {reply, From, {ok, undefined}},
    {next_state, connected, Session#session{zone = ZonePID}, [Reply]};
active(EventType, EventContent, Session) ->
    handle_event(EventType, EventContent, Session).

% Fall-throughs

handle_event({call, From}, {set_id, SessionID}, Session) ->
    Reply = {reply, From, {ok, SessionID}},
    {keep_state, Session#session{id = SessionID}, [Reply]};
handle_event({call, From}, get_id, Session) ->
    Reply = {reply, From, Session#session.id},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_proxy, undefined}, Session) ->
    Reply = {reply, From, {ok, undefined}},
    Timeout = Session#session.disconnect_timeout,
    erlang:send_after(Timeout, self(), maybe_terminate),
    {next_state, disconnected, Session#session{proxy = undefined}, [Reply]};
handle_event({call, From}, {set_proxy, ProxyPID}, Session) ->
    Reply = {reply, From, {ok, ProxyPID}},
    {keep_state, Session#session{proxy = ProxyPID}, [Reply]};
handle_event({call, From}, get_proxy, Session) ->
    Reply = {reply, From, Session#session.proxy},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_serializer, Serializer}, Session) ->
    Reply = {reply, From, {ok, Serializer}},
    {keep_state, Session#session{serializer = Serializer}, [Reply]};
handle_event({call, From}, get_serializer, Session) ->
    Reply = {reply, From, Session#session.serializer},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_latency, Latency}, Session) ->
    Reply = {reply, From, {ok, Latency}},
    {keep_state, Session#session{latency = Latency}, [Reply]};
handle_event({call, From}, get_latency, Session) ->
    Reply = {reply, From, Session#session.latency},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_game_data, GameData}, Session) ->
    Reply = {reply, From, {ok, GameData}},
    {keep_state, Session#session{game_data = GameData}, [Reply]};
handle_event({call, From}, get_game_data, Session) ->
    Reply = {reply, From, Session#session.game_data},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_disconnect_callback, MFArgs}, Session) ->
    Reply = {reply, From, {ok, MFArgs}},
    {keep_state, Session#session{disconnect_callback = MFArgs}, [Reply]};
handle_event({call, From}, get_disconnect_callback, Session) ->
    Reply = {reply, From, Session#session.disconnect_callback},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_token, Token}, Session) ->
    Reply = {reply, From, {ok, Token}},
    {keep_state, Session#session{token = Token}, [Reply]};
handle_event({call, From}, get_token, Session) ->
    Reply = {reply, From, Session#session.token},
    {keep_state, Session, [Reply]};
handle_event({call, From}, {set_zone, Zone}, Session) ->
    logger:debug("Cannot connect to zone ~p: not connected", [Zone]),
    Reply = {reply, From, {error, not_connected}},
    {keep_state, Session, [Reply]};
handle_event({call, From}, get_zone, Session) ->
    Reply = {reply, From, Session#session.zone},
    {keep_state, Session, [Reply]};
handle_event(info, maybe_terminate, Session) ->
    {keep_state, Session};
handle_event(_, _, Session) ->
    {keep_state, Session}.

terminate(_Reason, _State, _Data) ->
    ok.
