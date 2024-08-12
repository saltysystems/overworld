%%=========================================================================
%% Overworld Session
%%
%% This module defines and holds a session's state
%%
%%=========================================================================
-module(ow_session).
-behaviour(gen_server).

-define(SERVER(SessionID),
    {via, gproc, {n, l, {?MODULE, SessionID}}}
).

% API
-export([
    start/2,
    start/1,
    stop/1,
    pid/2, pid/1,
    serializer/2, serializer/1,
    latency/2, latency/1,
    game_data/2, game_data/1,
    termination_callback/2, termination_callback/1,
    status/2, status/1,
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

-include_lib("eunit/include/eunit.hrl").

%-type msg() :: nonempty_binary() | [binary(), ...].
-type serializer() :: undefined | protobuf.
-type id() :: pos_integer().
-type status() :: undefined | connected | disconnected.
-type time_ms() :: non_neg_integer().

-record(session, {
    pid :: pid() | undefined,
    serializer :: serializer(),
    latency :: time_ms(),
    game_data :: any(),
    termination_callback :: mfa() | undefined,
    disconnect_timeout :: time_ms(),
    status :: status(),
    token :: binary() | undefined,
    zone :: pid() | undefined
}).
-export_type([serializer/0]).
-export_type([id/0]).

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Start the session server and create a new session with this ID
%% @end
%%----------------------------------------------------------------------------
-spec start(id()) -> gen_server:start_ret().
start(ID) ->
    start(ID, []).
-spec start(id(), [tuple()]) -> gen_server:start_ret().
start(ID, Config) ->
    gen_server:start_link(?SERVER(ID), ?MODULE, [Config], []).

%%----------------------------------------------------------------------------
%% @doc Stop the session server
%% @end
%%----------------------------------------------------------------------------
stop(ID) ->
    gen_server:stop(?SERVER(ID)).

%%----------------------------------------------------------------------------
%% @doc Set the process id of the websocket or enet handler
%% @end
%%----------------------------------------------------------------------------
-spec pid(pid(), id()) -> {ok, pid()}.
pid(Pid, ID) ->
    gen_server:call(?SERVER(ID), {set_pid, Pid}).

%%----------------------------------------------------------------------------
%% @doc Get the process id of the websocket or enet handler
%% @end
%%----------------------------------------------------------------------------
-spec pid(id()) -> pid() | undefined.
pid(ID) ->
    gen_server:call(?SERVER(ID), get_pid).

%%----------------------------------------------------------------------------
%% @doc Set the format for serializing data. If communication happens all
%%      within Erlang node(s), then there is no need to set a serializer.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(serializer(), id()) -> {ok, serializer()}.
serializer(Serializer, ID) ->
    gen_server:call(?SERVER(ID), {set_serializer, Serializer}).

%%----------------------------------------------------------------------------
%% @doc Get the format for serializing data.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(id()) -> serializer() | undefined.
serializer(ID) ->
    gen_server:call(?SERVER(ID), get_serializer).

%%----------------------------------------------------------------------------
%% @doc Set the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(pos_integer(), id()) -> {ok, pos_integer()}.
latency(Latency, ID) ->
    gen_server:call(?SERVER(ID), {set_latency, Latency}).

%%----------------------------------------------------------------------------
%% @doc Get the session latency
%% @end
%%----------------------------------------------------------------------------
-spec latency(id()) -> non_neg_integer().
latency(ID) ->
    gen_server:call(?SERVER(ID), get_latency).

%%----------------------------------------------------------------------------
%% @doc Set the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(any(), id()) -> {ok, any()}.
game_data(Data, ID) ->
    gen_server:call(?SERVER(ID), {set_game_data, Data}).

%%----------------------------------------------------------------------------
%% @doc Get the game data
%% @end
%%----------------------------------------------------------------------------
-spec game_data(id()) -> any().
game_data(ID) ->
    gen_server:call(?SERVER(ID), get_game_info).

%%----------------------------------------------------------------------------
%% @doc Set the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec termination_callback(mfa(), id()) -> {ok, mfa()}.
termination_callback(Callback, ID) ->
    gen_server:call(?SERVER(ID), {set_termination_callback, Callback}).

%%----------------------------------------------------------------------------
%% @doc Get the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec termination_callback(id()) -> undefined | mfa().
termination_callback(ID) ->
    gen_server:call(?SERVER(ID), get_termination_callback).

%%----------------------------------------------------------------------------
%% @doc Set the connection status. Disconnected sessions will be periodically
%%      culled.
%% @end
%%----------------------------------------------------------------------------
-spec status(status(), id()) -> {ok, status()}.
status(Status, ID) ->
    gen_server:call(?SERVER(ID), {set_status, Status}).

%%----------------------------------------------------------------------------
%% @doc Get the connection status. Disconnected sessions will be periodically
%%      culled.
%% @end
%%----------------------------------------------------------------------------
-spec status(id()) -> status().
status(ID) ->
    gen_server:call(?SERVER(ID), get_status).

%%----------------------------------------------------------------------------
%% @doc Sets the session token
%% @end
%%----------------------------------------------------------------------------
-spec token(binary(), id()) -> {ok, binary()}.
token(Token, ID) ->
    gen_server:call(?SERVER(ID), {set_token, Token}).

%%----------------------------------------------------------------------------
%% @doc Get the session token.
%% @end
%%----------------------------------------------------------------------------
-spec token(id()) -> binary() | undefined.
token(ID) ->
    gen_server:call(?SERVER(ID), get_token).

%%----------------------------------------------------------------------------
%% @doc Sets the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(pid(), id()) -> {ok, pid()}.
zone(Zone, ID) ->
    gen_server:call(?SERVER(ID), {set_zone, Zone}).

%%----------------------------------------------------------------------------
%% @doc Get the zone pid
%% @end
%%----------------------------------------------------------------------------
-spec zone(id()) -> pid() | undefined.
zone(ID) ->
    gen_server:call(?SERVER(ID), get_token).

%%=========================================================================
%% Callback handlers
%%=========================================================================

init([Config]) ->
    Session = #session{
        pid = proplists:get_value(pid, Config, undefined),
        serializer = proplists:get_value(serializer, Config, undefined),
        latency = proplists:get_value(latency, Config, 0),
        game_data = proplists:get_value(game_data, Config, undefined),
        termination_callback = proplists:get_value(
            termination_callback, Config, undefined
        ),
        disconnect_timeout = proplists:get_value(
            disconnect_timeout, Config, 30000
        ),
        status = proplists:get_value(status, Config, undefined),
        token = proplists:get_value(token, Config, undefined),
        zone = proplists:get_value(zone, Config, undefined)
    },
    % Set a timer to maybe terminate a stale session if it hasn't gone active
    % within the default timeout period
    Timeout = Session#session.disconnect_timeout,
    erlang:send_after(Timeout, self(), maybe_terminate),
    {ok, Session}.

handle_call({set_pid, Pid}, _From, Session) ->
    {reply, {ok, Pid}, Session#session{pid = Pid}};
handle_call(get_pid, _From, Session) ->
    {reply, Session#session.pid, Session};
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
handle_call({set_termination_callback, TCB}, _From, Session) ->
    {reply, {ok, TCB}, Session#session{termination_callback = TCB}};
handle_call(get_termination_callback, _From, Session) ->
    {reply, Session#session.termination_callback, Session};
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
    {reply, Session#session.token, Session}.

handle_cast(_Msg, Session) ->
    {noreply, Session}.

handle_info(maybe_terminate, Session = #session{status = S}) when
    S =:= disconnected;
    S =:= undefined
->
    % Client is still disconnected, terminate.
    logger:notice(
        "Disconnecting session ~p in state ~p after hitting timeout", [
            self(), S
        ]
    ),
    {stop, timeout, Session};
handle_info(maybe_terminate, Session) ->
    % Client is back into a connected state, continue on as normal.
    {noreply, Session};
handle_info(_Info, Session) ->
    {noreply, Session}.

terminate(_Reason, _Session) ->
    ok.

code_change(_OldVsn, Session, _Extra) ->
    {ok, Session}.

%%=========================================================================
%% Internal functions
%%=========================================================================
