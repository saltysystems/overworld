%%%-------------------------------------------------------------------
%% @doc Overworld supervisor for client sessions
%% @end
%%%-------------------------------------------------------------------

-module(ow_session_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    new/2,
    delete/1
]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec new(ow_session:id(), [tuple()]) -> supervisor:startchild_ret().
new(SessionID, Config) ->
    Spec = #{
        id => SessionID,
        start => {ow_session, start, [SessionID, Config]}
    },
    supervisor:start_child(?SERVER, Spec).

-spec delete(ow_session:id()) -> ok | {error, _}.
delete(SessionID) ->
    supervisor:terminate_child(?SERVER, SessionID),
    supervisor:delete_child(?SERVER, SessionID).

init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    % Zone Options
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
