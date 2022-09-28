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
    start/1,
    stop/0
]).

start(PeerInfo) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PeerInfo], []).

stop() ->
    gen_server:stop(?SERVER).

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
    % Add a new key to the peerInfo map containing Overworld session
    % information
    {ok, PeerInfo#{session => S3}}.

handle_call({call, Msg}, _From, State) ->
    {reply, Msg, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({enet, disconnected, remote, _Pid, _When}, State) ->
    #{session := Session, ip := RawIP} = State,
    IP = inet:ntoa(RawIP),
    logger:info("ENet session ~p disconnected", [IP]),
    case ow_session:get_termination_callback(Session) of
        {M, F, 1} ->
            erlang:apply(M, F, [Session]);
        _ ->
            ok
    end,
    {stop, normal, State};
handle_info(
    {enet, Channel, {reliable, Msg}},
    State = #{channels := Channels, session := Session}
) ->
    ChannelPid = maps:get(Channel, Channels),
    S1 =
        case ow_protocol:decode(Msg, Session) of
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
                FlatMsg = iolist_to_binary(Msg1),
                enet:send_reliable(ChannelPid, FlatMsg),
                Session1
        end,
    {noreply, State#{session := S1}};
handle_info({enet, _Channel, {unreliable, Seq, Packet}}, State) ->
    logger:debug("Got an unreliable packet: ~p:~p", [Seq, Packet]),
    {noreply, State};
handle_info({enet, _Channel, {unsequenced, Group, Packet}}, State) ->
    logger:debug("Got a message from another process: ~p:~p", [
        Group, Packet
    ]),
    {noreply, State};
handle_info({_From, Type, Msg}, State = #{channels := Channels}) when
    Type =:= 'broadcast'; Type =:= 'zone_msg'
->
    % Handle a message from another overworld process
    % TODO: Thread through channel
    ChannelPID = maps:get(0, Channels),
    %       selection
    FlatMsg = iolist_to_binary(Msg),
    enet:send_reliable(ChannelPID, FlatMsg),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
