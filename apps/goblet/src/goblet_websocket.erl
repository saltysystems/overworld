-module(goblet_websocket).

-behavior(cowboy_handler).

%-export([init/2]).
-export([init/2, terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-include("goblet_opcode.hrl").

%%============================================================================
%% Cowboy Handler Callbacks
%%============================================================================

%%----------------------------------------------------------------------------
%% @doc Handler is initialized for any new connection and logs the foreign IP
%% @end
%%----------------------------------------------------------------------------
init(Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: client connected.", [IP]),
    {cowboy_websocket, Req, State}.

%%----------------------------------------------------------------------------
%% @doc Terminate callback for cleanup processes
%% @end
%%----------------------------------------------------------------------------
terminate(_Reason, Req, _State) ->
    #{peer := {IP, _Port}} = Req,
    logger:notice("~p: client disconnected", [IP]),
    ok.

%%----------------------------------------------------------------------------
%% @doc Set up the initial state of the websocket handler
%%      The greeting handler is spawned, which can be helpful debug
%%      information sent to the client to ensure that the connection
%%      (preferably over TLS) is working correctly.
%% @end
%%----------------------------------------------------------------------------
websocket_init(State) ->
    Msg = goblet_protocol:player_log("CONNECTION ESTABLISHED"),
    {reply, {binary, Msg}, State}.

%%----------------------------------------------------------------------------
%% @doc The websocket handler passes any binary message down to the protocol
%%      decoder for further processing. All other messages are discarded.
%% @end
%%----------------------------------------------------------------------------
websocket_handle({binary, Msg}, State) ->
    % protocol decoding will reply with an 'ok' for asynchronous messages or
    % will give us a binary to send back to the client
    case goblet_protocol:decode(Msg, State) of
        {ok, NewState} ->
            {ok, NewState};
        {Msg1, NewState} ->
            {reply, {binary, Msg1}, NewState}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% @doc websocket_info is triggered when a message from another erlang process
%%      comes into this handler process. At the moment we just shunt that back
%%      to the client.
%% @end
%%----------------------------------------------------------------------------
% if we get a message sent to our Pid that looks like something from another
% process (or myself), reply it back to the client
websocket_info({_Pid, event, Msg}, State) ->
    logger:notice("Received event, forwarding to client"),
    {reply, {binary, Msg}, State};
websocket_info(Info, State) ->
    logger:notice("Got a message from another process: ~p", [Info]),
    {ok, State}.
