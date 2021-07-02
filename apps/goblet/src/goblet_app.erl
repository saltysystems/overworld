%%%-------------------------------------------------------------------
%% @doc goblet public API
%% @end
%%%-------------------------------------------------------------------

-module(goblet_app).

-behaviour(application).

-export([start/2, stop/1]).

-record(session, {email = none, authenticated = false, match = false}).

start(_StartType, _StartArgs) ->
    % Try to setup items
    goblet_db:create_item(
        "Reactor MK I",
        1,
        move,
        'SELF',
        0,
        0,
        none,
        [visible],
        100
    ),
    goblet_db:create_item(
        "Missile MK I",
        2,
        missile,
        'DIRECT',
        40,
        0,
        none,
        [],
        100
    ),
    goblet_db:create_item(
        "Beam MK I",
        4,
        beam,
        'LINEAR',
        60,
        0,
        none,
        [],
        100
    ),
    goblet_db:create_item(
        "Repair MK I",
        2,
        repair,
        'DIRECT',
        0,
        25,
        none,
        [],
        100
    ),
    goblet_db:create_item(
        "Scanner MK I",
        1,
        scan,
        'AREA',
        0,
        0,
        none,
        [],
        100
    ),

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
