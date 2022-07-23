%%%-------------------------------------------------------------------
%% @doc Overworld Public API
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).

-behaviour(application).

-export([start/2, stop/1]).

% status information via JSON API
-export([status/0]).

%-record(session, {email = none, authenticated = false, match = false}).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ow_websocket, []},
            {"/client/download", ow_dl_handler, []},
            {"/stats", ow_stats, []},
            {"/lib_ow.gd", cowboy_static,
                {file, "apps/ow_core/static/lib_ow.gd"}}
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
    ow_sup:start_link().

stop(_State) ->
    ok.

status() ->
    {ok, Version} = application:get_key(ow, vsn),
    {ok, Description} = application:get_key(ow, description),
    #{
        <<"name">> => <<"ow">>,
        <<"version">> => erlang:list_to_binary(Version),
        <<"Description">> => erlang:list_to_binary(Description)
    }.
