%%%-------------------------------------------------------------------
%% @doc Overworld Public API
%% @end
%%%-------------------------------------------------------------------

-module(ow_app).

-behaviour(application).

-export([start/2, stop/1]).

% status information via JSON API
-export([status/0]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ow_websocket, []},
            {"/client/download", ow_dl_handler, []},
            {"/stats", ow_stats, []}
            % TODO - See if this is needed, I think it's old.
            %{"/libow.gd", cowboy_static,
            %    {file, "apps/ow_core/static/libow.gd"}}
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
    {ok, Version} = application:get_key(overworld, vsn),
    {ok, Description} = application:get_key(overworld, description),
    #{
        <<"name">> => <<"overworld">>,
        <<"version">> => erlang:list_to_binary(Version),
        <<"Description">> => erlang:list_to_binary(Description)
    }.
