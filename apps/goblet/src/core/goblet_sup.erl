%%%-------------------------------------------------------------------
%% @doc goblet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(goblet_sup).

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
        goblet_account,
        goblet_chat,
        goblet_game_msg,
        goblet_session
    ],
    ChildSpecs = [
        %#{id => goblet_lobby, start => {goblet_lobby, start, []}},
        #{
            id => goblet_protocol2,
            start => {goblet_protocol2, start, [Modules]}
        },
        %#{
        %    id => goblet_instance_sup,
        %    start => {goblet_instance_sup, start_link, []}
        %},
        #{
            id => goblet_script_sup,
            start => {goblet_script_sup, start_link, []}
        }
        %,
        %        #{
        %            id => goblet_entity_sup,
        %            start => {goblet_entity_sup, start_link, []}
        %        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
