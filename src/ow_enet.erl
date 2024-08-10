-module(ow_enet).
-behaviour(gen_server).

% API
-export([
    start/1
]).

% Required gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-type qos() :: {
    reliable | unreliable | unsequenced | undefined,
    non_neg_integer() | undefined
}.
-export_type([qos/0]).

%%===========================================================================
%% API
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Starts the ENet connection handler with any initial state
%% @end
%%---------------------------------------------------------------------------
-spec start(map()) -> gen_server:start_ret().
start(PeerInfo) ->
    gen_server:start_link(?MODULE, [PeerInfo], []).

%%===========================================================================
%% Callbacks
%%===========================================================================

%%---------------------------------------------------------------------------
%% @doc Handler is initialized for any new connection and logs the foreign IP
%% @end
%%---------------------------------------------------------------------------
-spec init([map()]) -> {ok, map()}.
init([PeerInfo]) ->
    IP = inet:ntoa(maps:get(ip, PeerInfo)),
    logger:notice("Starting ENet session for ~p", [IP]),
    SessionID = erlang:unique_integer([positive]),
    logger:notice("~p: Pending session ID: ~p", [SessionID]),
    % Register the process by this SessionID in gproc
    gproc:reg({n, l, SessionID}, ignored),
    % Trap exits from the enet child processes
    process_flag(trap_exit, true),
    % Add a new key to the peerInfo map containing Overworld session
    % information
    {ok, PeerInfo#{session_id => SessionID}}.

% Required ballbacks
handle_call({call, Msg}, _From, PeerInfo) ->
    {reply, Msg, PeerInfo}.
handle_cast(_Msg, PeerInfo) ->
    {noreply, PeerInfo}.

% Handle messages from other processes
handle_info({enet, disconnected, remote, _Pid, _When}, PeerInfo) ->
    #{ip := RawIP} = PeerInfo,
    IP = inet:ntoa(RawIP),
    logger:notice("~p: ENet client disconnected", [IP]),
    {stop, normal, PeerInfo};
handle_info({enet, Channel, {reliable, Msg}}, PeerInfo) ->
    decode_and_reply(
        Msg, Channel, {enet, send_reliable}, PeerInfo
    ),
    {noreply, PeerInfo};
handle_info({enet, Channel, {unreliable, _Seq, Msg}}, PeerInfo) ->
    decode_and_reply(Msg, Channel, {enet, send_unreliable}, PeerInfo),
    {noreply, PeerInfo};
handle_info({enet, Channel, {unsequenced, _Group, Msg}}, PeerInfo) ->
    decode_and_reply(Msg, Channel, {enet, send_unsequenced}, PeerInfo),
    {noreply, PeerInfo};
handle_info(
    {_From, Type, Msg, Options}, PeerInfo = #{channels := Channels}
) when
    Type =:= 'broadcast'; Type =:= 'zone_msg'
->
    % Handle a message from another overworld process
    channelize_msg(Msg, Channels, Options),
    {noreply, PeerInfo};
handle_info({reconnect_session, SessionID1}, PeerInfo) ->
    #{session_id := SessionID} = PeerInfo,
    ow_session:reconnect(SessionID, SessionID1),
    {noreply, PeerInfo#{session_id => SessionID1}};
handle_info(_, PeerInfo) ->
    % Ignore all other messages
    {noreply, PeerInfo}.

terminate(_Reason, _PeerInfo) ->
    % We've caught an error or otherwise asked to stop, clean up the session
    ok.

code_change(_OldVsn, PeerInfo, _Extra) -> {ok, PeerInfo}.

%%===========================================================================
%% Internal functions
%%===========================================================================

decode_and_reply(Msg, IncomingChannel, {Mod, Fun}, PeerInfo) ->
    #{channels := Channels, session_id := SessionID} = PeerInfo,
    case ow_protocol:route(Msg, SessionID) of
        ok ->
            ok;
        {Msg1, {QOS, MsgChannel}} ->
            channelize_msg(Msg1, Channels, {QOS, MsgChannel});
        Msg1 ->
            % Default to sending a reliable message on whatever channel the
            % original message came in on
            FlatMsg = iolist_to_binary(Msg1),
            ChannelPid = maps:get(IncomingChannel, Channels),
            erlang:apply(Mod, Fun, [ChannelPid, FlatMsg])
    end.

channelize_msg(Msg, Channels, {QOS, Channel}) ->
    % Not sure if it's worth implementing any fall-throughs here, better to
    % crash early if someone fat-fingers the QOS or channel number rather than
    % to unexpectedly send reliable,0 messages.
    ChannelPID = maps:get(Channel, Channels),
    FlatMsg = iolist_to_binary(Msg),
    case QOS of
        reliable ->
            enet:send_reliable(ChannelPID, FlatMsg);
        unreliable ->
            enet:send_unreliable(ChannelPID, FlatMsg);
        unsequenced ->
            enet:send_unsequenced(ChannelPID, FlatMsg)
    end.
