%%%-------------------------------------------------------------------
%% @doc goblet public API
%% @end
%%%-------------------------------------------------------------------

-module(goblet_app).

-behaviour(application).

-export([start/2, stop/1]).

-record(session, {email = none, authenticated = false, match = false}).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", goblet_websocket, #session{}}
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
    goblet_sup:start_link().

stop(_State) ->
    ok.
