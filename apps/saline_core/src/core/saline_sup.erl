%%%-------------------------------------------------------------------
%% @doc saline top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(saline_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    Modules = [
        saline_account,
        saline_session,
        saline_beacon
    ],
    ChildSpecs = [
        #{
            id => saline_protocol,
            start => {saline_protocol, start, [Modules]}
        },
        #{
            id => saline_script_sup,
            start => {saline_script_sup, start_link, []}
        },
        #{
            id => saline_beacon,
            start => {saline_beacon, start, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
