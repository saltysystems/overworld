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
    {ok, SessionPid} = ow_session_sup:new([{proxy, self()}]),
    logger:debug("Started session ~p for ENet conn. ~p", [SessionPid, IP]),
    % Trap exits from the enet child processes
    process_flag(trap_exit, true),
    % Add a new key to the peerInfo map containing Overworld session
    % information
    {ok, PeerInfo#{session_pid => SessionPid}}.

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
handle_info({enet, _Channel, {reliable, Msg}}, PeerInfo) ->
    decode_and_reply(Msg, PeerInfo),
    {noreply, PeerInfo};
handle_info({enet, _Channel, {unreliable, _Seq, Msg}}, PeerInfo) ->
    decode_and_reply(Msg, PeerInfo),
    {noreply, PeerInfo};
handle_info({enet, _Channel, {unsequenced, _Group, Msg}}, PeerInfo) ->
    decode_and_reply(Msg, PeerInfo),
    {noreply, PeerInfo};
handle_info(
    {_From, ow_msg, {MsgType, Msg}}, PeerInfo = #{channels := Channels}
) ->
    % Handle a message from another overworld process
    channelize_msg(MsgType, Msg, Channels),
    {noreply, PeerInfo};
handle_info({reconnect_session, SessionPID}, PeerInfo) ->
    {noreply, PeerInfo#{session_pid := SessionPID}};
handle_info(Msg, PeerInfo) ->
    logger:warning("Unimplemented response for message: ~p", [Msg]),
    % Ignore all other messages
    {noreply, PeerInfo}.

%%---------------------------------------------------------------------------
%% @doc Clean up the ENet handler by calling the session's disconnect callback
%% @end
%%---------------------------------------------------------------------------
-spec terminate(any(), peerinfo()) -> ok.
terminate(_Reason, #{session_pid := SessionPID}) ->
    ok = ow_session_util:disconnect(SessionPID).

code_change(_OldVsn, PeerInfo, _Extra) -> {ok, PeerInfo}.

%%===========================================================================
%% Internal functions
%%===========================================================================

decode_and_reply(Msg, PeerInfo) ->
    #{channels := Channels, session_pid := SessionPID} = PeerInfo,
    %TODO: Fix me
    case ow_protocol:route(Msg, SessionPID) of
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
