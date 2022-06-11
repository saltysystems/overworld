%%%-------------------------------------------------------------------
%% @doc saline public API
%% @end
%%%-------------------------------------------------------------------

-module(saline_app).

-behaviour(application).

-export([start/2, stop/1]).

%-record(session, {email = none, authenticated = false, match = false}).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", saline_websocket, []},
            {"/client/download", saline_dl_handler, []},
            {"/stats", saline_stats, []},
            {"/libsaline.gd", cowboy_static,
                {file, "apps/saline_core/static/libsaline.gd"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [
            {port, 4433},
            {nodelay, true}
        ],
        #{env => #{dispatch => Dispatch}}
    ),
    saline_sup:start_link().

stop(_State) ->
    ok.
