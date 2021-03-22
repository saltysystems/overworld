-module(goblet_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => goblet_instance,
            start => {goblet_instance, start, []},
            shutdown => brutal_kill
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
