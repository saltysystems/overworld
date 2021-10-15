-module(gremlin_script_sup).

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
            id => gremlin_script_srv,
            start => {gremlin_script_srv, start, []},
            restart => transient
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
