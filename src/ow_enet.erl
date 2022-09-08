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
    {ok, PeerInfo}.

handle_call({call, Msg}, _From, State) ->
    {reply, Msg, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({enet, disconnected, remote, Pid, _When}, State) ->
    logger:debug("Got disconnect from: ~p. Stopping peer.", [Pid]),
    {stop, normal, State};
handle_info({enet, _Channel, {reliable, _Packet}}, State) ->
    {noreply, State};
handle_info({enet, _Channel, {unreliable, Seq, Packet}}, State) ->
    logger:debug("Got an unreliable packet: ~p:~p", [Seq, Packet]),
    {noreply, State};
handle_info({enet, _Channel, {unsequenced, Group, Packet}}, State) ->
    logger:debug("Got a message from another process: ~p:~p", [
        Group, Packet
    ]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
