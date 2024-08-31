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
-export([connect/1, reconnect/2, notify_clients/3]).

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
-spec session_ping(map(), ow_session:id()) -> {atom(), map()}.
session_ping(Msg, SessionID) ->
    BeaconID = maps:get(id, Msg),
    Last = ow_beacon:get_by_id(BeaconID),
    Now = erlang:monotonic_time(),
    % NOTE: This change to RTT compared to ow_session (v1)
    Latency = erlang:convert_time_unit(
        round(Now - Last), native, millisecond
    ),
    {ok, Latency} = ow_session:latency(Latency, SessionID),
    {session_pong, #{latency => Latency}}.

%%----------------------------------------------------------------------------
%% @doc Request a new session, or rejoin an existing one
%% @end
%%----------------------------------------------------------------------------
-spec session_request(map(), ow_session:id()) -> ok.
session_request(Msg, SessionID) ->
    logger:notice("Got session request: ~p", [Msg]),
    Token = maps:get(token, Msg, undefined),
    case Token of
        undefined ->
            % No session existing, start a new one
            logger:notice("No session, starting a new one for ~p", [
                SessionID
            ]),
            {ok, Pid} = ow_session_sup:new(SessionID, [{pid, self()}]),
            logger:notice("Started session: ~p:~p", [SessionID, Pid]),
            NewToken = ow_token_serv:new(SessionID),
            Reply = #{id => SessionID, reconnect_token => NewToken},
            notify_clients(session_new, Reply, [SessionID]);
        _ ->
            {SessionID1, NewToken} = ow_token_serv:exchange(Token),
            % Lookup the PID of the handler (Enet or Websocket) and ask it to
            % update its session ID
            Pid = gproc:lookup_pid({n, l, SessionID}),
            Pid ! {reconnect_session, SessionID1},
            % Call the zone and let it know that the client has reconnected
            ZonePid = ow_session:zone(SessionID),
            ow_zone:reconnect(ZonePid, SessionID),
            reconnect(SessionID, SessionID1),
            % Update the Session server with the new token
            {ok, NewToken} = ow_session:token(NewToken, SessionID)
    end,
    % Set the status to connected
    ow_session:status(connected, SessionID),
    % Register the process
    gproc:reg({p, l, client_session}),
    ok.

%%===========================================================================
%% Utility API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Register the caller's Pid in gproc with a key of SessionID
%% @end
%%----------------------------------------------------------------------------
-spec connect(ow_session:id()) -> ok.
connect(SessionID) ->
    gproc:reg({n, l, SessionID}, ignored),
    ok.

%%----------------------------------------------------------------------------
%% @doc Unregister an old SessionID and register a new SessionID for the caller
%% @end
%%----------------------------------------------------------------------------
-spec reconnect(ow_session:id(), ow_session:id()) -> ok.
reconnect(SessionID, SessionID1) ->
    logger:debug("Reconnecting session: ~p -> ~p", [SessionID, SessionID1]),
    % Register the process by this SessionID in gproc
    gproc:reg({n, l, SessionID1}, ignored),
    % Unregister the old value in gproc
    gproc:unreg({n, l, SessionID}),
    ok.

-spec notify_clients(atom(), map(), [ow_session:id()]) -> ok.
notify_clients(_MsgType, _Msg, []) ->
    ok;
notify_clients(MsgType, Msg, [SessionID | Rest]) ->
    try
        Pid = ow_session:pid(SessionID),
        case Pid of
            undefined ->
                ok;
            _ ->
                % Send a message to the client, let the connection handler figure
                % out how to serialzie it further
                Pid ! {self(), client_msg, {MsgType, Msg}}
        end
    catch
        exit:{noproc, _} ->
            ok
    end,
    notify_clients(MsgType, Msg, Rest).
