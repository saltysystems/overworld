%%=========================================================================
%% Overworld Session
%%
%% This module handles communication with game sessions.
%%
%%=========================================================================
-module(ow_session).
-behaviour(gen_server).

-define(SERVER(SessionID),
    {via, gproc, {n, l, {?MODULE, SessionID}}}
).
-define(DEFAULT_CONNECTION_TIMEOUT, 60000). % ms

% API
-export([
    start/2,
    start/1,
    stop/1,
    connect/1,
    reconnect/2,
    pid/2, pid/1,
    serializer/2,serializer/1,
    latency/2, latency/1,
    game_data/2, game_data/1,
    termination_callback/2, termination_callback/1,
    status/2, status/1,
    token/2, token/1
]).

% RPC callbacks
-export([ 
         session_ping/2,
         session_request/2
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

-record(session, {
    pid :: pid() | undefined,
    serializer :: serializer(),
    latency :: non_neg_integer(), % in milliseconds
    game_data :: any(), % it can be anything but we default to map
    termination_callback :: mfa() | undefined,
    status :: status(),
    token :: binary()
}).

-export_type([serializer/0]).

%%----------------------------------------------------------------------------
%% RPC annotation for Client library generation
%%
%%----------------------------------------------------------------------------

-rpc_encoder(#{app => overworld, lib => overworld_pb, interface => ow_msg}).
-rpc_client([session_beacon, session_pong]).
-rpc_server([session_request, session_ping]).

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Calculate the latency based on the RTT to the client
%% @end
%%----------------------------------------------------------------------------
-spec session_ping([binary(), ...], id()) -> binary().
session_ping(Msg, SessionID) ->
    BeaconID = maps:get(id, Msg),
    Last = ow_beacon:get_by_id(BeaconID),
    Now = erlang:monotonic_time(),
    % NOTE: This change to RTT compared to ow_session (v1)
    Latency = erlang:convert_time_unit(
        round(Now - Last), native, millisecond
    ),
    {ok, Latency} = latency(Latency, SessionID),
    ow_msg:encode(#{latency => Latency}, session_pong).

%%----------------------------------------------------------------------------
%% @doc Request a new session, or rejoin an existing one
%% @end
%%----------------------------------------------------------------------------
-spec session_request([binary(), ...], id()) -> ok.
session_request(Msg, SessionID) ->
    Token = maps:get(token, Msg, undefined),
    case Token of
        undefined ->
            % No session existing, start a new one
            start(SessionID);
        _ -> 
            {SessionID1, NewToken} = ow_token_serv:exchange(Token),
            % Lookup the PID of the handler (Enet or Websocket) and ask it to
            % update its session ID
            Pid = gproc:lookup_pid({n,l,SessionID}),
            Pid ! {reconnect_session, SessionID1},
            % Set the connection state to active
            {ok, connected} = status(connected, SessionID),
            % Update the Session server with the new token
            {ok, NewToken} = token(NewToken, SessionID)
    end,
    ok.

%%----------------------------------------------------------------------------
%% @doc Start the session server and create a new session with this ID
%% @end
%%----------------------------------------------------------------------------
-spec start(id()) -> gen_server:start_ret().
start(ID) -> 
    start([], ID).
-spec start([tuple()], id()) -> gen_server:start_ret().
start(Config, ID) ->
    gen_server:start_link(?SERVER(ID), ?MODULE, [], [Config]).

%%----------------------------------------------------------------------------
%% @doc Stop the session server 
%% @end
%%----------------------------------------------------------------------------
stop(ID) -> 
    gen_server:stop(?SERVER(ID)).

%%----------------------------------------------------------------------------
%% @doc Register the caller's Pid in gproc with a key of SessionID
%% @end
%%----------------------------------------------------------------------------
-spec connect(pos_integer()) -> ok.
connect(SessionID) ->
    gproc:reg({n,l,SessionID}, ignored),
    ok.

%%----------------------------------------------------------------------------
%% @doc Unregister an old SessionID and register a new SessionID for the caller
%% @end
%%----------------------------------------------------------------------------
-spec reconnect(pos_integer(), pos_integer()) -> ok.
reconnect(SessionID, SessionID1) ->
    logger:debug("Reconnecting session: ~p -> ~p", [SessionID, SessionID1]),
    % Register the process by this SessionID in gproc
    gproc:reg({n, l, SessionID1}, ignored),
    % Unregister the old value in gproc
    gproc:unreg({n,l,SessionID}),
    ok.

%%----------------------------------------------------------------------------
%% @doc Set the process id of the websocket or enet handler
%% @end
%%----------------------------------------------------------------------------
-spec pid(pid(), id()) -> {ok, pid()}.
pid(Pid,ID) -> 
    gen_server:call(?SERVER(ID), {set_pid, Pid}).

%%----------------------------------------------------------------------------
%% @doc Get the process id of the websocket or enet handler
%% @end
%%----------------------------------------------------------------------------
-spec pid(id()) -> pid().
pid(ID) -> 
    gen_server:call(?SERVER(ID), get_pid).

%%----------------------------------------------------------------------------
%% @doc Set the format for serializing data. If communication happens all
%%      within Erlang node(s), then there is no need to set a serializer.
%% @end
%%----------------------------------------------------------------------------
-spec serializer(serializer(),id()) -> {ok, serializer()}.
serializer(Serializer, ID) ->
    gen_server:call(?SERVER(ID), {set_serializer, Serializer}).

%%----------------------------------------------------------------------------
%% @doc Get the format for serializing data. 
%% @end
%%----------------------------------------------------------------------------
-spec serializer(id()) -> serializer().
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
-spec token(id()) -> binary().
token(ID) ->
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
       termination_callback = proplists:get_value(termination_callback, Config, undefined),
       status = proplists:get_value(status, Config, undefined)
      },
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

handle_call({set_status, Status}, _From, Session) when Status =:= disconnected ->
    % On a disconnected session, set a timer to terminate this session
    erlang:send_after(?DEFAULT_CONNECTION_TIMEOUT, self(), maybe_terminate),
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

handle_info(maybe_terminate, Session) when Session#session.status =:= disconnected ->
    % Client is still disconnected, terminate.
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
