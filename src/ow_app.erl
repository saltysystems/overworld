%%%-------------------------------------------------------------------
%% @doc Overworld Public API
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(OVERWORLD_WEBSOCKET_PORT, 4434).
-define(OVERWORLD_WEBSOCKET_TLS_PORT, 4435).
-define(OVERWORLD_ENET_PORT, 4484).
-define(OVERWORLD_ENET_DTLS_PORT, 4485).
-define(OVERWORLD_ENET_CHANNEL_LIMIT, 1).
-define(OVERWORLD_ENET_PEER_LIMIT, 4).

% status information via JSON API
-export([status/0]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ow_websocket, []},
            {"/client/download", ow_dl_handler, []},
            {"/stats", ow_stats, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, ?OVERWORLD_WEBSOCKET_PORT},
            {nodelay, true}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    % Enet Info
    Port = ?OVERWORLD_ENET_PORT,
    Handler = {ow_enet, start, []},
    Options = [
        {channel_limit, ?OVERWORLD_ENET_CHANNEL_LIMIT},
        {peer_limit, ?OVERWORLD_ENET_PEER_LIMIT}
    ],
    {ok, _} = enet:start_host(Port, Handler, Options),
    ow_sup:start_link().

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
