%%%-------------------------------------------------------------------
%% @doc goblet public API
%% @end
%%%-------------------------------------------------------------------

-module(goblet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", goblet_websocket, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http,
        [{port, 4434}],
        #{env => #{dispatch => Dispatch}}
    ),
    goblet_sup:start_link().

stop(_State) ->
    ok.
