-module(saline_enet_sup).
%-behaviour(supervisor).

-export([start/0, stop/0]).

-define(PEER_LIMIT, 64).
-define(CHANNEL_LIMIT, 4).

start() ->
    Port = 4484,
    Handler = {saline_enet, start, []},
    Options = [{peer_limit, ?PEER_LIMIT}, {channel_limit, 4}],
    enet:start_host(Port, Handler, Options).

stop() ->
    enet:stop_host(4484).
