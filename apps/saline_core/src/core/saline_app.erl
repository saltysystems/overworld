%%%-------------------------------------------------------------------
%% @doc saline public API
%% @end
%%%-------------------------------------------------------------------

-module(saline_app).

-behaviour(application).

-export([start/2, stop/1]).

% status information via JSON API
-export([status/0]).

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

status() ->
    {ok, Version} = application:get_key(saline, vsn),
    {ok, Description} = application:get_key(saline, description),
    #{
        <<"name">> => <<"saline">>,
        <<"version">> => erlang:list_to_binary(Version),
        <<"Description">> => erlang:list_to_binary(Description)
    }.
