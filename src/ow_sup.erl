%%%-------------------------------------------------------------------
%% @doc ow top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ow_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
            id => ow_player_reg,
            start => {ow_player_reg, start_link, []}
        },
        #{
            id => ow_beacon,
            start => {ow_beacon, start, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
