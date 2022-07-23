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
        ow_account,
        ow_session,
        ow_beacon
    ],
    ChildSpecs = [
        #{
            id => ow_protocol,
            start => {ow_protocol, start, [Modules]}
        },
        #{
            id => ow_script_sup,
            start => {ow_script_sup, start_link, []}
        },
        #{
            id => ow_beacon,
            start => {ow_beacon, start, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
