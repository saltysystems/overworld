%%%-------------------------------------------------------------------
%% @doc Overworld Application
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(PEER_LIMIT, "64").
-define(CHANNEL_LIMIT, "4").
-define(ENET_PORT, "4484").
-define(WS_PORT, "4434").

-spec make_config(map()) -> map().
make_config(StartArgs) ->
    % For each default, check for the appropriate environment variable or
    % define
    Cfg = #{
        peer_limit =>
            list_to_integer(os:getenv("OW_PEER_LIMIT", ?PEER_LIMIT)),
        channel_limit =>
            list_to_integer(os:getenv("OW_CHANNEL_LIMIT", ?CHANNEL_LIMIT)),
        enet_port =>
            list_to_integer(os:getenv("OW_ENET_PORT", ?ENET_PORT)),
        ws_port =>
            list_to_integer(os:getenv("OW_WS_PORT", ?WS_PORT))
    },
    maps:merge(Cfg, StartArgs).

-spec start(application:start_type(), map()) -> supervisor:startlink_ret().
start(_StartType, StartArgs) ->
    #{
        ws_port := WsPort,
        enet_port := EnetPort,
        peer_limit := PeerLimit,
        channel_limit := ChannelLimit
    } = make_config(StartArgs),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ow_websocket, []},
            {"/client/download", ow_dl_handler, []},
            {"/client/manifest", ow_dl_manifest, []}
        ]}
    ]),
    % Start WebSocket/CowBoy
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, WsPort},
            % turn off TCP_NODELAY for better game perf
            {nodelay, true}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    % Start ENet
    Options = [
        {peer_limit, PeerLimit},
        {channel_limit, ChannelLimit},
        {compression_mode, zlib}
    ],
    Handler = {ow_enet, start, []},
    enet:start_host(EnetPort, Handler, Options),
    % Start the OW supervisor
    SuperLink = ow_sup:start_link(),
    % Now register the initial application and modules before returning
    Application = #{
        app => overworld,
        prefix => 100,
        router => ow_msg,
        modules => [ow_account, ow_session, ow_beacon]
    },
    ow_protocol:register(Application),
    SuperLink.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
