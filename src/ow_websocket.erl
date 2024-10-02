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
    {ok, Pid} = ow_session_sup:new([{proxy, self()}]),
    logger:debug("Started session ~p for WebSocket conn. ~p", [Pid, IP]),
    {cowboy_websocket, Req, Pid}.

%%---------------------------------------------------------------------------
%% @doc Terminate callback for cleanup processes
%% @end
%%---------------------------------------------------------------------------
-spec terminate(any(), cowboy_req:req(), pid()) -> ok.
terminate(_Reason, Req, Pid) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: WebSocket client disconnected", [IP]),
    ok = ow_session_util:disconnect(Pid).

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
websocket_handle({binary, Msg}, Pid) ->
    % protocol decoding will reply with an 'ok' for asynchronous messages or
    % will give us a binary to send back to the client
    case ow_protocol:route(Msg, Pid) of
        ok ->
            {ok, Pid};
        {MsgType, Msg1} ->
            {reply, {binary, to_binary(MsgType, Msg1)}, Pid}
    end;
websocket_handle(Frame, Pid) ->
    logger:debug("Received some other kind of frame: ~p", [Frame]),
    {ok, Pid}.

%%--------------------------------------------------------------------------
%% @doc websocket_info is triggered when a message from another erlang
%%      process comes into this handler process.
%% @end
%%--------------------------------------------------------------------------
-spec websocket_info({From, client_msg, Msg}, SessionPid) -> Reply when
    From :: pid(),
    Msg :: binary(),
    SessionPid :: pid(),
    Reply :: {reply, {binary, Msg1}, SessionPid1},
    Msg1 :: binary(),
    SessionPid1 :: pid().
websocket_info({_From, client_msg, {MsgType, Msg}}, Pid) ->
    % Encode the message into binary
    Bin = to_binary(MsgType, Msg),
    {reply, {binary, Bin}, Pid};
websocket_info({reconnect_session, Pid1}, _Pid) ->
    {ok, Pid1};
websocket_info(Info, Session) ->
    logger:notice("Got a message from another process: ~p", [Info]),
    {ok, Session}.

to_binary(MsgType, Msg) ->
    #{encoder := Encoder} = ow_protocol:rpc(MsgType, client),
    #{interface := EncoderMod, app := App, lib := EncoderLib} = Encoder,
    erlang:apply(EncoderMod, encode, [Msg, MsgType, EncoderLib, App]).
