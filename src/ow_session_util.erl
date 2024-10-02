%%=========================================================================
%% Overworld Session Utilities
%%
%% This module holds various RPCs for sessions and utilty functions for
%% handling sessions and session-related messages
%%
%%=========================================================================

-module(ow_session_util).
% RPC functions
-export([session_ping/2, session_request/2]).
% Utility functions
-export([disconnect/1, notify_clients/3]).

%%===========================================================================
%% RPC API
%%===========================================================================

-rpc_encoder(#{app => overworld, lib => overworld_pb, interface => ow_msg}).
-rpc_client([session_beacon, session_new, session_pong]).
-rpc_server([session_request, session_ping]).

%%----------------------------------------------------------------------------
%% @doc Calculate the latency based on the RTT to the client
%% @end
%%----------------------------------------------------------------------------
-spec session_ping(map(), pid()) -> {atom(), map()}.
session_ping(Msg, SessionPID) ->
    BeaconID = maps:get(id, Msg),
    Last = ow_beacon:get_by_id(BeaconID),
    Now = erlang:monotonic_time(),
    % NOTE: This change to RTT compared to ow_session (v1)
    Latency = erlang:convert_time_unit(
        round(Now - Last), native, millisecond
    ),
    {ok, Latency} = ow_session:latency(Latency, SessionPID),
    {session_pong, #{latency => Latency}}.

%%----------------------------------------------------------------------------
%% @doc Request a new session, or rejoin an existing one
%% @end
%%----------------------------------------------------------------------------
-spec session_request(map(), pid()) -> ok.
session_request(Msg, SessionPID) ->
    logger:notice("Got session request: ~p", [Msg]),
    Token = maps:get(token, Msg, undefined),
    ProxyPID = ow_session:proxy(SessionPID),
    case Token of
        undefined ->
            % No session existing, start a new one
            NewToken = ow_token_serv:new(SessionPID),
            ID = ow_session:id(SessionPID),
            Reply = #{id => ID, reconnect_token => NewToken},
            % Send the reply back through the proxy
            notify_clients(session_new, Reply, [ProxyPID]);
        _ ->
            {PrevSessionPID, NewToken} = ow_token_serv:exchange(Token),
            % Inform the proxy process to update its session ID to refer to the
            % previous, existing one. Internal clients will probably(?) never
            % need to do this
            ProxyPID ! {reconnect_session, PrevSessionPID},
            % Inform the zone that the client has reconnected
            ZonePid = ow_session:zone(PrevSessionPID),
            ow_zone:reconnect(ZonePid, PrevSessionPID),
            % Update the session server with the new token
            {ok, NewToken} = ow_session:token(NewToken, PrevSessionPID),
            % Stop the temporary session
            ok = ow_session_sup:delete(SessionPID)
    end,
    % Set the status to connected
    ow_session:connected(SessionPID),
    % Register the process of the caller
    gproc:reg({p, l, client_session}),
    ok.

%%===========================================================================
%% Utility API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Set the session to disconnected state and run the appropriate callback
%%      handler
%% @end
%%----------------------------------------------------------------------------
-spec disconnect(pid()) -> ok.
disconnect(SessionPID) ->
    % We've caught an error or otherwise asked to stop, clean up the session
    case ow_session:disconnect_callback(SessionPID) of
        {Module, Fun, Args} ->
            logger:notice("Calling: ~p:~p(~p)", [Module, Fun, Args]),
            erlang:apply(Module, Fun, Args);
        undefined ->
            ok
    end,
    {ok, disconnected} = ow_session:disconnected(SessionPID),
    ok.

%%----------------------------------------------------------------------------
%% @doc Send a message to a list of clients
%% @end
%%----------------------------------------------------------------------------
-spec notify_clients(atom(), map(), [pid()]) -> ok.
notify_clients(_MsgType, _Msg, []) ->
    ok;
notify_clients(MsgType, Msg, [SessionPID | Rest]) ->
    try
        ProxyPID = ow_session:proxy(SessionPID),
        case ProxyPID of
            undefined ->
                ok;
            _ ->
                % Send a message to the client, let the connection handler figure
                % out how to serialize it further
                ProxyPID ! {self(), client_msg, {MsgType, Msg}}
        end
    catch
        exit:{noproc, _} ->
            ok
    end,
    notify_clients(MsgType, Msg, Rest).
