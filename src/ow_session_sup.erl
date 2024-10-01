%%%-------------------------------------------------------------------
%% @doc Overworld supervisor for client sessions
%% @end
%%%-------------------------------------------------------------------

-module(ow_session_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    new/1,
    delete/1
]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec new([tuple()]) -> supervisor:startchild_ret().
new(Config) ->
    supervisor:start_child(?SERVER, [Config]).

-spec delete(ow_session:id()) -> ok | {error, _}.
delete(SessionID) ->
    supervisor:terminate_child(?SERVER, SessionID),
    supervisor:delete_child(?SERVER, SessionID).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    % Session
    ChildSpecs = [
        #{
            id => session,
            start => {ow_session, start, []},
            restart => transient
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
