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
% api
-export([
    start/1,
    stop/0
]).

-define(SERVER, ?MODULE).

% API
start(PeerInfo) ->
    gen_server:start_link(?MODULE, [PeerInfo], []).

stop() ->
    gen_server:stop(?SERVER).

% callbacks

init([PeerInfo]) ->
    IP = inet:ntoa(maps:get(ip, PeerInfo)),
    logger:notice("~p: client connected (Enet)", [IP]),
    SessionID = erlang:unique_integer(),
    St1 = ow_session:set_id(SessionID, ow_session:new()),
    St2 = ow_session:set_pid(self(), St1),
    St3 = ow_session:set_serializer(protobuf, St2),
    St4 = ow_session:set_peer_info(PeerInfo, St3),
    {ok, St4}.

handle_call({call, Msg}, _From, State) ->
    {reply, Msg, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({enet, disconnected, remote, _Pid, _When}, State) ->
    PeerInfo = ow_session:get_peer_info(State),
    IP = inet:ntoa(maps:get(ip, PeerInfo)),
    logger:notice("~p: client disconnected (Enet)", [IP]),
    {stop, normal, State};
handle_info({enet, Channel, {reliable, Msg}}, Session) ->
    process_response(Msg, Channel, "reliable", Session);
handle_info({enet, Channel, {unreliable, _Seq, Msg}}, Session) ->
    process_response(Msg, Channel, "unreliable", Session);
handle_info({enet, Channel, {unsequenced, _Group, Msg}}, Session) ->
    process_response(Msg, Channel, "unsequenced", Session);
handle_info({zone_msg, RPC, Msg}, Session) ->
    % TODO: Thread me through
    Channel = 0,
    PeerInfo = ow_session:get_peer_info(Session),
    ChannelServers = maps:get(channels, PeerInfo),
    ChannelPID = maps:get(Channel, ChannelServers),
    % Check the RPC for a dispatch
    case ow_rpc:dispatch(RPC) of
        unreliable ->
            enet:send_unreliable(ChannelPID, Msg);
        unsequenced ->
            enet:send_unsequenced(ChannelPID, Msg);
        _ ->
            % Assume reliable, this matches WebSocket
            enet:send_reliable(ChannelPID, Msg)
    end,
    {noreply, Session}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_response(Msg, Channel, Dispatch, Session) ->
    case ow_protocol:decode(Msg, Session) of
        ok ->
            {noreply, Session};
        {ok, Session1} ->
            {noreply, Session1};
        {Msg1, Session1} ->
            % Reply with the dispatch type that we were sent.
            Send = erlang:list_to_existing_atom("send_" ++ Dispatch),
            enet:Send(Channel, Msg1),
            {noreply, Session1}
    end.
