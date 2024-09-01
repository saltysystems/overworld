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

-type peerinfo() :: map().
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
-spec init([peerinfo()]) -> {ok, peerinfo()}.
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
    {_From, client_msg, {MsgType, Msg}}, PeerInfo = #{channels := Channels}
) ->
    % Handle a message from another overworld process
    channelize_msg(MsgType, Msg, Channels),
    {noreply, PeerInfo};
handle_info({reconnect_session, SessionID1}, PeerInfo) ->
    #{session_id := SessionID} = PeerInfo,
    ow_session_util:reconnect(SessionID, SessionID1),
    {noreply, PeerInfo#{session_id => SessionID1}};
handle_info(_, PeerInfo) ->
    % Ignore all other messages
    {noreply, PeerInfo}.

%%---------------------------------------------------------------------------
%% @doc Clean up the ENet handler by calling the session's disconnect callback
%% @end
%%---------------------------------------------------------------------------
-spec terminate(any(), peerinfo()) -> ok.
terminate(_Reason, #{session_id := SessionID}) ->
    % We've caught an error or otherwise asked to stop, clean up the session
    case ow_session:disconnect_callback(SessionID) of
        {Module, Fun, Args} ->
            logger:notice("Calling: ~p:~p(~p)", [Module, Fun, Args]),
            erlang:apply(Module, Fun, Args);
        undefined ->
            ok
    end,
    {ok, disconnected} = ow_session:status(disconnected, SessionID),
    ok.

code_change(_OldVsn, PeerInfo, _Extra) -> {ok, PeerInfo}.

%%===========================================================================
%% Internal functions
%%===========================================================================

% TODO: If we look up the channel/qos information from the module definition, I
%       guess it's not necessary to pass a module/fun to this
%       But I am a bit worried about chopping down this particular tree so far
%       afield right now.
decode_and_reply(Msg, _IncomingChannel, _MF, PeerInfo) ->
    #{channels := Channels, session_id := SessionID} = PeerInfo,
    %TODO: Fix me
    case ow_protocol:route(Msg, SessionID) of
        ok ->
            ok;
        {MsgType, Msg1} ->
            channelize_msg(MsgType, Msg1, Channels)
    end.

channelize_msg(MsgType, Msg, Channels) ->
    #{encoder := Encoder, channel := Channel, qos := QOS} = ow_protocol:rpc(
        MsgType, client
    ),
    #{interface := EncoderMod, app := App, lib := EncoderLib} = Encoder,
    EncodedMsg = erlang:apply(EncoderMod, encode, [
        Msg, MsgType, EncoderLib, App
    ]),
    FlatMsg = iolist_to_binary(EncodedMsg),
    ChannelPID = maps:get(Channel, Channels),
    case QOS of
        reliable ->
            enet:send_reliable(ChannelPID, FlatMsg);
        unreliable ->
            enet:send_unreliable(ChannelPID, FlatMsg);
        unsequenced ->
            enet:send_unsequenced(ChannelPID, FlatMsg)
    end.
