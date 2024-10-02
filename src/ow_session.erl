%%=========================================================================
%% Overworld Session
%%
%% This module defines and holds a session's state
%%
%%=========================================================================
-module(ow_session).
-behaviour(gen_server).

% API
-export([
    start/1,
    start/0,
    stop/1,
    id/2, id/1,
    proxy/2, proxy/1,
    serializer/2, serializer/1,
    latency/2, latency/1,
    game_data/2, game_data/1,
    disconnect_callback/2, disconnect_callback/1,
    status/1,
    connected/1,
    disconnected/1,
    token/2, token/1,
    zone/2, zone/1
]).

% gen_server required callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%-type msg() :: nonempty_binary() | [binary(), ...].
-type serializer() :: undefined | protobuf.
-type id() :: pos_integer().
-type token() :: binary() | undefined.
-type status() :: preconnect | connected | disconnected.
-type time_ms() :: non_neg_integer().
-type mfargs() :: {atom(), atom(), list()}.

-record(session, {
    % internal use only
    id :: id(),
    proxy :: pid() | undefined,
    serializer :: serializer(),
    latency :: time_ms(),
    game_data :: any(),
    disconnect_callback :: mfargs() | undefined,
    disconnect_timeout :: time_ms(),
    status :: status(),
    token :: token(),
    zone :: pid() | undefined
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
    gen_server:start(?MODULE, [Config], []).

%%----------------------------------------------------------------------------
%% @doc Stop the session server
%% @end
%%----------------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%----------------------------------------------------------------------------
%% @doc Set the session ID for this session (network serializable)
%% @end
%%----------------------------------------------------------------------------
-spec id(pid(), id()) -> {ok, id()}.
id(PID, SessionID) ->
    gen_server:call(PID, {set_id, SessionID}).

%%----------------------------------------------------------------------------
%% @doc Get the session id of this session (network serializable)
%% @end
%%----------------------------------------------------------------------------
-spec id(pid()) -> id() | undefined.
id(PID) ->
    gen_server:call(PID, get_id).

%%----------------------------------------------------------------------------
%% @doc Set the pid of the session's proxy process, including ENet, WebSocket
%%      and internal handlers.
%% @end
%%----------------------------------------------------------------------------
-spec proxy(pid(), pid()) -> {ok, pid()}.
proxy(PID, ProxyPID) ->
    gen_server:call(PID, {set_proxy, ProxyPID}).

%%----------------------------------------------------------------------------
%% @doc Get the pid of the session's proxy process.
%% @end
%%----------------------------------------------------------------------------
-spec proxy(pid()) -> pid() | undefined.
proxy(PID) ->
    gen_server:call(PID, get_proxy).

%%----------------------------------------------------------------------------
%% @doc Set the format for serializing data. If communication happens all
%%      within Erlang node(s), then there is no need to set a serializer.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(serializer(), pid()) -> {ok, serializer()}.
serializer(Serializer, PID) ->
    gen_server:call(PID, {set_serializer, Serializer}).

%%----------------------------------------------------------------------------
%% @doc Get the format for serializing data.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(pid()) -> serializer() | undefined.
serializer(PID) ->
    gen_server:call(PID, get_serializer).

%%----------------------------------------------------------------------------
%% @doc Set the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(pos_integer(), pid()) -> {ok, pos_integer()}.
latency(Latency, PID) ->
    gen_server:call(PID, {set_latency, Latency}).

%%----------------------------------------------------------------------------
%% @doc Get the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(pid()) -> non_neg_integer().
latency(PID) ->
    gen_server:call(PID, get_latency).

%%----------------------------------------------------------------------------
%% @doc Set the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(any(), pid()) -> {ok, any()}.
game_data(Data, PID) ->
    gen_server:call(PID, {set_game_data, Data}).

%%----------------------------------------------------------------------------
%% @doc Get the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(pid()) -> any().
game_data(PID) ->
    gen_server:call(PID, get_game_info).

%%----------------------------------------------------------------------------
%% @doc Set the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec disconnect_callback(mfargs(), pid()) -> {ok, mfargs()}.
disconnect_callback(Callback, PID) ->
    gen_server:call(PID, {set_disconnect_callback, Callback}).

%%----------------------------------------------------------------------------
%% @doc Get the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec disconnect_callback(pid()) -> undefined | mfargs().
disconnect_callback(PID) ->
    gen_server:call(PID, get_disconnect_callback).

%%----------------------------------------------------------------------------
%% @doc Set the current session to disconnected state
%% @end
%%----------------------------------------------------------------------------
-spec disconnected(pid()) -> {ok, status()}.
disconnected(PID) ->
    gen_server:call(PID, {set_status, disconnected}).

%%----------------------------------------------------------------------------
%% @doc Set the current session to connected state
%% @end
%%----------------------------------------------------------------------------
-spec connected(pid()) -> {ok, status()}.
connected(PID) ->
    gen_server:call(PID, {set_status, connected}).

%%----------------------------------------------------------------------------
%% @doc Get the connection status. Disconnected sessions will be periodically
%%      culled.
%% @end
%%----------------------------------------------------------------------------
-spec status(pid()) -> status().
status(PID) ->
    gen_server:call(PID, get_status).

%%----------------------------------------------------------------------------
%% @doc Sets the session token
%% @end
%%----------------------------------------------------------------------------
-spec token(token(), pid()) -> {ok, token()}.
token(Token, PID) ->
    gen_server:call(PID, {set_token, Token}).

%%----------------------------------------------------------------------------
%% @doc Get the session token.
%% @end
%%----------------------------------------------------------------------------
-spec token(pid()) -> token().
token(PID) ->
    gen_server:call(PID, get_token).

%%----------------------------------------------------------------------------
%% @doc Sets the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(pid(), pid()) -> {ok, pid()}.
zone(ZonePID, PID) ->
    gen_server:call(PID, {set_zone, ZonePID}).

%%----------------------------------------------------------------------------
%% @doc Get the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(pid()) -> pid() | undefined.
zone(PID) ->
    gen_server:call(PID, get_zone).

%%=========================================================================
%% Callback handlers
%%=========================================================================

init([Config]) ->
    Session = #session{
        id = proplists:get_value(id, Config, erlang:unique_integer([positive])),
        serializer = proplists:get_value(serializer, Config, undefined),
        latency = proplists:get_value(latency, Config, 0),
        game_data = proplists:get_value(game_data, Config, undefined),
        disconnect_callback = proplists:get_value(
            disconnect_callback, Config, undefined
        ),
        disconnect_timeout = proplists:get_value(
            disconnect_timeout, Config, ?DEFAULT_DISCONNECT_TIMEOUT
        ),
        status = proplists:get_value(status, Config, preconnect),
        token = proplists:get_value(token, Config, undefined),
        zone = proplists:get_value(zone, Config, undefined)
    },
    % Register with gproc
    true = gproc:reg({n, l, Session#session.id}, ignored),
    % Set a timer to maybe terminate a stale session if it hasn't gone active
    % within the default timeout period
    Timeout = Session#session.disconnect_timeout,
    erlang:send_after(Timeout, self(), maybe_terminate),
    {ok, Session}.

handle_call({set_id, SessionID}, _From, Session) ->
    {reply, {ok, SessionID}, Session#session{id = SessionID}};
handle_call(get_id, _From, Session) ->
    {reply, Session#session.id, Session};
handle_call({set_proxy, ProxyPID}, _From, Session) ->
    {reply, {ok, ProxyPID}, Session#session{proxy = ProxyPID}};
handle_call(get_proxy, _From, Session) ->
    {reply, Session#session.proxy, Session};
handle_call({set_serializer, Serializer}, _From, Session) ->
    {reply, {ok, Serializer}, Session#session{serializer = Serializer}};
handle_call(get_serializer, _From, Session) ->
    {reply, Session#session.serializer, Session};
handle_call({set_latency, Latency}, _From, Session) ->
    {reply, {ok, Latency}, Session#session{latency = Latency}};
handle_call(get_latency, _From, Session) ->
    {reply, Session#session.latency, Session};
handle_call({set_game_data, GameData}, _From, Session) ->
    {reply, {ok, GameData}, Session#session{game_data = GameData}};
handle_call(get_game_data, _From, Session) ->
    {reply, Session#session.game_data, Session};
handle_call({set_disconnect_callback, TCB}, _From, Session) ->
    {reply, {ok, TCB}, Session#session{disconnect_callback = TCB}};
handle_call(get_disconnect_callback, _From, Session) ->
    {reply, Session#session.disconnect_callback, Session};
handle_call({set_status, Status}, _From, Session) when
    Status =:= disconnected
->
    % On a disconnected session, set a timer to terminate this session
    Timeout = Session#session.disconnect_timeout,
    erlang:send_after(Timeout, self(), maybe_terminate),
    {reply, {ok, Status}, Session#session{status = Status}};
handle_call({set_status, Status}, _From, Session) ->
    {reply, {ok, Status}, Session#session{status = Status}};
handle_call(get_status, _From, Session) ->
    {reply, Session#session.status, Session};
handle_call({set_token, Token}, _From, Session) ->
    {reply, {ok, Token}, Session#session{token = Token}};
handle_call(get_token, _From, Session) ->
    {reply, Session#session.token, Session};
handle_call({set_zone, Zone}, _From, Session) ->
    {reply, {ok, Zone}, Session#session{zone = Zone}};
handle_call(get_zone, _From, Session) ->
    {reply, Session#session.zone, Session}.

handle_cast(_Msg, Session) ->
    {noreply, Session}.

handle_info(maybe_terminate, Session = #session{status = S, zone = Z}) when
    S =:= disconnected;
    S =:= preconnect
->
    case Z of
        undefined ->
            {stop, normal, Session};
        _ ->
            erlang:send_after(?DEFAULT_SHUTDOWN_DELAY, self(), terminate)
    end;
handle_info(maybe_terminate, Session) ->
    % Client is back into a connected state, continue on as normal.
    {noreply, Session};
handle_info(terminate, Session) ->
    {stop, normal, Session};
handle_info(_Info, Session) ->
    {noreply, Session}.

terminate(Reason, _Session) ->
    logger:notice("Terminating session ~p for reason ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, Session, _Extra) ->
    {ok, Session}.
