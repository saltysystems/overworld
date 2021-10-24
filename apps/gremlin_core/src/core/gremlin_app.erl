%%%-------------------------------------------------------------------
%% @doc gremlin public API
%% @end
%%%-------------------------------------------------------------------

-module(gremlin_app).

-behaviour(application).

-export([start/2, stop/1]).

-record(session, {email = none, authenticated = false, match = false}).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", gremlin_websocket, #session{}},
            {"/libgremlin.gd", cowboy_static,
                {file, "apps/gremlin_core/static/libgremlin.gd"}}
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
    gremlin_sup:start_link().

stop(_State) ->
    ok.
