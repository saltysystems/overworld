%%%-------------------------------------------------------------------
%% @doc Overworld Application
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(PEER_LIMIT, "64").
-define(CHANNEL_LIMIT, "4").
-define(ENET_PORT, "4483").
-define(WS_PORT, "4433").
-define(WSS_PORT, "4434").
-define(OW_PREFIX, 100).

-spec start(application:start_type(), map()) -> supervisor:startlink_ret().
start(StartType, []) ->
    start(StartType, #{});
start(_StartType, StartArgs) ->
    #{
        ws_port := WsPort,
        wss_port := WssPort,
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
    % Start Cowboy for Websocket connections
    start_http(WsPort, Dispatch),
    CertFile = os:getenv("OW_TLS_CERT"),
    KeyFile = os:getenv("OW_TLS_KEY"),
    start_https(WssPort, CertFile, KeyFile, Dispatch),
    % Start ENet for UDP connections
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
        prefix => ?OW_PREFIX,
        router => ow_msg,
        modules => [ow_session, ow_beacon]
    },
    ow_protocol:register(Application),
    SuperLink.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec start_http(integer(), cowboy_router:dispatch_rules()) -> ok.
start_http(Port, Dispatch) ->
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, Port},
            % turn off TCP_NODELAY for better game perf
            {nodelay, true}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.

-spec start_https(
    integer(), string(), string(), cowboy_router:dispatch_rules()
) -> ok.
start_https(Port, CertFile, KeyFile, Dispatch) ->
    case CertFile of
        false ->
            logger:warning("Not starting HTTPS, no certfile provided");
        _ ->
            case KeyFile of
                false ->
                    logger:warning(
                        "Not starting HTTPS, no keyfile provided"
                    );
                _ ->
                    Result = cowboy:start_tls(
                        https,
                        [
                            {port, Port},
                            % turn off TCP_NODELAY for better game perf
                            {nodelay, true},
                            {certfile, CertFile},
                            {keyfile, KeyFile}
                        ],
                        #{env => #{dispatch => Dispatch}}
                    ),
                    case Result of
                        {ok, _} ->
                            ok;
                        Error ->
                            logger:error("Failed to start HTTPS: ~p", [
                                Error
                            ])
                    end
            end
    end.

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
            list_to_integer(os:getenv("OW_WS_PORT", ?WS_PORT)),
        wss_port =>
            list_to_integer(os:getenv("OW_WSS_PORT", ?WSS_PORT))
    },
    maps:merge(Cfg, StartArgs).
