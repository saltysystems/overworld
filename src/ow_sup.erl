%%%-------------------------------------------------------------------
%% @doc ow top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ow_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
%% @doc Starts the top-level supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
%% @doc Initializes the supervisor with child specifications.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    ChildSpecs = [
        #{
            id => ow_protocol,
            start => {ow_protocol, start, []}
        },
        #{
            id => ow_session_sup,
            start => {ow_session_sup, start_link, []}
        },
        #{
            id => ow_token_serv,
            start => {ow_token_serv, start_link, []}
        },
        #{
            id => ow_beacon,
            start => {ow_beacon, start, []}
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
