-module(gremlin_websocket).

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
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: client connected.", [IP]),
    SessionID = erlang:unique_integer(),
    gproc:reg({p, l, client_session}, SessionID),
    St1 = gremlin_session:set_id(SessionID, gremlin_session:new()),
    {cowboy_websocket, Req, St1}.

%%---------------------------------------------------------------------------
%% @doc Terminate callback for cleanup processes
%% @end
%%---------------------------------------------------------------------------
terminate(_Reason, Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: client disconnected", [IP]),
    logger:debug("Client state at disconnect: ~p", [State]),
    ok.

%%---------------------------------------------------------------------------
%% @doc Set up the initial state of the websocket handler
%%      The greeting handler is spawned, which can be helpful debug
%%      information sent to the client to ensure that the connection
%%      (preferably over TLS) is working correctly.
%% @end
%%---------------------------------------------------------------------------
websocket_init(State) ->
    Msg = gremlin_session:encode_log("CONNECTION ESTABLISHED"),
    {reply, {binary, Msg}, State}.

%%---------------------------------------------------------------------------
%% @doc The websocket handler passes any binary message down to the protocol
%%      decoder for further processing. All other messages are discarded.
%% @end
%%---------------------------------------------------------------------------
-spec websocket_handle({binary, [binary(), ...]}, any()) -> ws_result().
websocket_handle({binary, Msg}, State) ->
    % protocol decoding will reply with an 'ok' for asynchronous messages or
    % will give us a binary to send back to the client
    case gremlin_protocol:decode(Msg, State) of
        {ok, NewState} ->
            {ok, NewState};
        {Msg1, NewState} ->
            {reply, {binary, Msg1}, NewState}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

%%--------------------------------------------------------------------------
%% @doc websocket_info is triggered when a message from another erlang
%%      process comes into this handler process. At the moment we just shunt
%%      that back to the client.
%% @end
%%--------------------------------------------------------------------------
websocket_info({_Pid, broadcast, Msg}, State) ->
    logger:notice("Received broadcast message, forwarding to client"),
    {reply, {binary, Msg}, State};
websocket_info({_Pid, multicast, Msg, Who}, State) ->
    ID = gremlin_session:get_id(State),
    case lists:member(ID, Who) of
        true ->
            logger:notice(
                "Received message for this client, forwarding to client"
            ),
            {reply, {binary, Msg}, State};
        false ->
            {ok, State}
    end;
websocket_info(Info, State) ->
    logger:notice("Got a message from another process: ~p", [Info]),
    {ok, State}.
