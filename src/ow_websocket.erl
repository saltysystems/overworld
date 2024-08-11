-module(ow_websocket).

-behavior(cowboy_handler).

-export([init/2, terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-type ws_result() :: {ok, any()} | {reply, [binary(), ...], any()}.

%%===========================================================================
%% Cowboy Handler Callbacks
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Handler is initialized for any new connection and logs the foreign IP
%% @end
%%---------------------------------------------------------------------------

-spec init(cowboy_req:req(), any()) ->
    {cowboy_websocket, cowboy_req:req(), any()}.
init(Req, _St0) ->
    #{peer := {RawIP, _Port}} = Req,
    IP = inet:ntoa(RawIP),
    logger:notice("Starting WebSocket session for ~p", [IP]),
    SessionID = erlang:unique_integer([positive]),
    ow_session:connect(SessionID),
    logger:notice("~p: Pending SessionID: ~p", [IP, SessionID]),
    {cowboy_websocket, Req, SessionID}.

%%---------------------------------------------------------------------------
%% @doc Terminate callback for cleanup processes
%% @end
%%---------------------------------------------------------------------------
-spec terminate(any(), cowboy_req:req(), any()) -> ok.
terminate(_Reason, Req, SessionID) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: WebSocket client disconnected", [IP]),
    logger:debug("Client session at disconnect: ~p", [SessionID]),
    ok.

%%---------------------------------------------------------------------------
%% @doc Set up the initial state of the websocket handler
%% @end
%%---------------------------------------------------------------------------
-spec websocket_init(any()) -> {ok, any()}.
websocket_init(State) ->
    {ok, State}.

%%---------------------------------------------------------------------------
%% @doc The websocket handler passes any binary message down to the protocol
%%      decoder for further processing. All other messages are discarded.
%% @end
%%---------------------------------------------------------------------------
-spec websocket_handle({binary, [binary(), ...]}, pos_integer()) ->
    ws_result().
websocket_handle({binary, Msg}, SessionID) ->
    % protocol decoding will reply with an 'ok' for asynchronous messages or
    % will give us a binary to send back to the client
    case ow_protocol:route(Msg, SessionID) of
        ok ->
            {ok, SessionID};
        {Msg1, _Options} ->
            {reply, {binary, Msg1}, SessionID}
    end;
websocket_handle(Frame, SessionID) ->
    logger:debug("Received some other kind of frame: ~p", [Frame]),
    {ok, SessionID}.

%%--------------------------------------------------------------------------
%% @doc websocket_info is triggered when a message from another erlang
%%      process comes into this handler process.
%% @end
%%--------------------------------------------------------------------------
-spec websocket_info({Pid, Type, Msg, Options}, SessionID) -> Reply when
    Pid :: pid(),
    Type :: broadcast | zone_msg,
    Msg :: binary(),
    Options :: any(),
    SessionID :: pos_integer(),
    Reply :: {reply, {binary, Msg1}, Session1},
    Msg1 :: binary(),
    Session1 :: ow_session:id().
websocket_info({_Pid, Type, Msg, _Options}, SessionID) when
    Type =:= 'broadcast'; Type =:= 'zone_msg'
->
    {reply, {binary, Msg}, SessionID};
websocket_info({reconnect_session, SessionID1}, SessionID) ->
    ow_session:reconnect(SessionID, SessionID1),
    {ok, SessionID1};
websocket_info(Info, Session) ->
    logger:debug("Got a message from another process: ~p", [Info]),
    {ok, Session}.
