%%%-------------------------------------------------------------------
%% @doc Overworld Public API
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).

-behaviour(application).

-export([start/2, stop/1]).

% status information via JSON API
-export([status/0]).

-define(PEER_LIMIT, 64).
-define(CHANNEL_LIMIT, 4).
-define(ENET_PORT, 4484).
-define(ENET_DTLS_PORT, 4485).
-define(WS_PORT, 4434).
-define(WS_TLS_PORT, 4435).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ow_websocket, []},
            {"/client/download", ow_dl_handler, []},
            {"/client/manifest", ow_dl_manifest, []},
            {"/stats", ow_stats, []}
        ]}
    ]),
    % Start WebSocket/CowBoy
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, ?WS_PORT},
            {nodelay, true}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    % Start ENet
    Options = [
        {peer_limit, ?PEER_LIMIT},
        {channel_limit, ?CHANNEL_LIMIT},
        {compression_mode, zlib}
    ],
    Handler = {ow_enet, start, []},
    enet:start_host(?ENET_PORT, Handler, Options),
    % Start the OW supervisor
    SuperLink = ow_sup:start_link(),
    % Now register the initial application and modules before returning
    Application = {overworld, {ow_msg, decode}},
    ow_protocol:register_app(100, Application),
    Modules = [ow_account, ow_session, ow_beacon],
    [ow_protocol:register_rpc(RPC) || RPC <- Modules],
    % Return the supervisor pid
    SuperLink.

stop(_State) ->
    ok.

status() ->
    {ok, Version} = application:get_key(overworld, vsn),
    {ok, Description} = application:get_key(overworld, description),
    #{
        <<"name">> => <<"overworld">>,
        <<"version">> => erlang:list_to_binary(Version),
        <<"Description">> => erlang:list_to_binary(Description)
    }.
