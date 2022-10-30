-module(ow_websocket).

-behavior(cowboy_handler).

%-export([init/2]).
-export([init/2, terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-type ws_result() :: {ok, any()} | {reply, [binary(), ...], any()}.

%%===========================================================================
%% Cowboy Handler Callbacks
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Handler is initialized for any new connection and logs the foreign IP
%% @end
%%---------------------------------------------------------------------------
init(Req, _St0) ->
    #{peer := {RawIP, _Port}} = Req,
    IP = inet:ntoa(RawIP),
    logger:notice("~p: client connected.", [IP]),
    SessionID = erlang:unique_integer(),
    St1 = ow_session:set_id(SessionID, ow_session:new()),
    St2 = ow_session:set_pid(self(), St1),
    St3 = ow_session:set_serializer(protobuf, St2),
    {cowboy_websocket, Req, St3}.

%%---------------------------------------------------------------------------
%% @doc Terminate callback for cleanup processes
%% @end
%%---------------------------------------------------------------------------
terminate(_Reason, Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: client disconnected", [IP]),
    logger:debug("Client state at disconnect: ~p", [State]),
    % Check for a termination callback in the client state
    case ow_session:get_termination_callback(State) of
        {M, F, 1} ->
            erlang:apply(M, F, [State]);
        _ ->
            ok
    end.

%%---------------------------------------------------------------------------
%% @doc Set up the initial state of the websocket handler
%%      The greeting handler is spawned, which can be helpful debug
%%      information sent to the client to ensure that the connection
%%      (preferably over TLS) is working correctly.
%% @end
%%---------------------------------------------------------------------------
websocket_init(State) ->
    gproc:reg({p, l, client_session}),
    St1 = ow_session:set_pid(self(), State),
    Msg = ow_session:encode_log("CONNECTION ESTABLISHED"),
    {reply, {binary, Msg}, St1}.

%%---------------------------------------------------------------------------
%% @doc The websocket handler passes any binary message down to the protocol
%%      decoder for further processing. All other messages are discarded.
%% @end
%%---------------------------------------------------------------------------
-spec websocket_handle(
    {binary, [binary(), ...]}, ow_session:session()
) -> ws_result().
websocket_handle({binary, Msg}, Session) ->
    % protocol decoding will reply with an 'ok' for asynchronous messages or
    % will give us a binary to send back to the client
    case ow_protocol:decode(Msg, Session) of
        % Table of possible returns
        %   ok -> No reply, keep old session
        %   {ok, Session1} -> No reply, update session
        %   {Msg1, Session1} -> Reply message, update session (which may
        %   ==Session)
        ok ->
            {ok, Session};
        {ok, Session1} ->
            {ok, Session1};
        {Msg1, Session1} ->
            {reply, {binary, Msg1}, Session1};
        {ok, Session1, _Options} ->
            % Options only for ENet for the moment
            {ok, Session1};
        {Msg1, Session1, _Options} ->
            % Options only for ENet for the moment
            {reply, {binary, Msg1}, Session1}
    end;
websocket_handle(Frame, State) ->
    logger:debug("Received some other kind of frame: ~p", [Frame]),
    {ok, State}.

%%--------------------------------------------------------------------------
%% @doc websocket_info is triggered when a message from another erlang
%%      process comes into this handler process.
%% @end
%%--------------------------------------------------------------------------
websocket_info({_Pid, Type, Msg, _Options}, Session) when
    Type =:= 'broadcast'; Type =:= 'zone_msg'
->
    {reply, {binary, Msg}, Session};
websocket_info(Info, State) ->
    logger:debug("Got a message from another process: ~p", [Info]),
    {ok, State}.
