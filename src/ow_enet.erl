-module(ow_enet).
-behaviour(gen_server).

% required gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

% api
-export([
    start/1
]).

-type qos() :: {
    reliable | unreliable | unsequenced | undefined,
    non_neg_integer() | undefined
}.
-export_type([qos/0]).

start(PeerInfo) ->
    gen_server:start_link(?MODULE, [PeerInfo], []).

% callbacks

init([PeerInfo]) ->
    IP = inet:ntoa(maps:get(ip, PeerInfo)),
    logger:info("New ENet connection from ~p", [IP]),
    ID = erlang:unique_integer(),
    S1 = ow_session:set_id(ID, ow_session:new()),
    S2 = ow_session:set_pid(self(), S1),
    S3 = ow_session:set_serializer(protobuf, S2),
    % Register this proces with gproc
    gproc:reg({p, l, client_session}),
    % Trap exits from the enet child processes
    process_flag(trap_exit, true),
    % Add a new key to the peerInfo map containing Overworld session
    % information
    {ok, PeerInfo#{session => S3}}.

handle_call({call, Msg}, _From, State) ->
    {reply, Msg, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({enet, disconnected, remote, _Pid, _When}, State) ->
    #{ip := RawIP} = State,
    IP = inet:ntoa(RawIP),
    logger:notice("ENet session ~p disconnected", [IP]),
    {stop, normal, State};
handle_info({enet, Channel, {reliable, Msg}}, State) ->
    S1 = decode_and_reply(Msg, Channel, {enet, send_reliable}, State),
    {noreply, State#{session := S1}};
handle_info({enet, Channel, {unreliable, _Seq, Msg}}, State) ->
    S1 = decode_and_reply(Msg, Channel, {enet, send_unreliable}, State),
    {noreply, State#{session := S1}};
handle_info({enet, Channel, {unsequenced, _Group, Msg}}, State) ->
    S1 = decode_and_reply(Msg, Channel, {enet, send_unsequenced}, State),
    {noreply, State#{session := S1}};
handle_info(
    {_From, Type, Msg, Options}, State = #{channels := Channels}
) when
    Type =:= 'broadcast'; Type =:= 'zone_msg'
->
    % Handle a message from another overworld process
    channelize_msg(Msg, Channels, Options),
    {noreply, State}.

terminate(_Reason, _State = #{session := Session}) ->
    % We've caught an error or otherwise asked to stop, clean up the session
    cleanup_session(Session),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

cleanup_session(Session) ->
    case ow_session:get_termination_callback(Session) of
        {M, F, 1} ->
            erlang:apply(M, F, [Session]);
        _ ->
            ok
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

decode_and_reply(Msg, IncomingChannel, {Mod, Fun}, State) ->
    #{channels := Channels, session := Session} = State,
    case ow_protocol:route(Msg, Session) of
        % Table of possible returns
        %   ok -> No reply, keep old session
        %   {ok, Session1} -> No reply, update session
        %   {Msg1, Session1} -> Reply message, update session (which may
        %   ==Session)
        ok ->
            Session;
        {ok, Session1} ->
            Session1;
        {Msg1, Session1} ->
            % Default to sending a reliable message on whatever channel the
            % original message came in on
            FlatMsg = iolist_to_binary(Msg1),
            ChannelPid = maps:get(IncomingChannel, Channels),
            erlang:apply(Mod, Fun, [ChannelPid, FlatMsg]),
            Session1;
        {ok, Session1, {_QOS, _MsgChannel}} ->
            Session1;
        {Msg1, Session1, {QOS, MsgChannel}} ->
            channelize_msg(Msg1, Channels, {QOS, MsgChannel}),
            Session1
    end.
