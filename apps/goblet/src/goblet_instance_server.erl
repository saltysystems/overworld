-module(goblet_instance_server).

%-behaviour(gen_server).

%-export([start/0, stop/0]).
-export([handle_tick/2, phases/1, normalize_actions/1]).

% Required genServer callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(action, {ap, name, from, target}).

% Process all inputs. If there aint nothin goin on, respond with the old state
% and don't update clients
handle_tick(Q, _InstanceState) when Q == [] ->
    ok;
handle_tick(_Q, _InstanceState) ->
    io:format("something else").

%handle_tick(Q, InstanceState) ->
%    % Pop one item off the stack and process it.
%	pqueue:

%is_valid_ability(Player, Ability) ->
%	% Check if the ability is in the player's ability list for this round
%	lists:member(Ability, Player#player.abilities).

normalize_actions(ActionList) ->
    % Take a list of actions for a player, e.g.
    %    [{1,shoot}, {2, move, {4, laz0r}]
    % and then normalize it such that the AP costs are translated into Phases:
    %  [{1,shoot}, {3, move}, {7, laz0r}]
    normalize_actions(ActionList, []).

normalize_actions([], Acc) ->
    lists:reverse(Acc);
normalize_actions([H | T], []) ->
    normalize_actions(T, [H]);
normalize_actions([H | T], [AccH | AccT]) when is_record(H, action) ->
    MP = H#action.ap + AccH#action.ap,
    MPList = H#action{ap = MP},
    normalize_actions(T, [MPList | [AccH | AccT]]);
normalize_actions([H | T], [AccH | AccT]) ->
    % H = 1, T = [2,3,4] ; AccH = 1, AccT = []
    NewList = H + AccH,
    normalize_actions(T, [NewList | [AccH | AccT]]).

% The ActionList must already do the math needed to handle this
phases(ActionList) ->
    % Get the largest key
    % AP must be the first item in the record, hence 2nd key
    S = lists:keysort(2, ActionList),
    % Get the last element of the list
    MaxPhase = lists:last(S),
    % Recurse through the phases, pairing up matching keys into the same phase
    % til we hit 0.
    group_phases(MaxPhase#action.ap, S).

group_phases(N, KeyList) ->
    group_phases(N, KeyList, []).

group_phases(N, _KeyList, Acc) when N == 0 ->
    Acc;
group_phases(N, KeyList, Acc) ->
    %PhaseGroup = [ X || {_, N1, _, _} = X <- KeyList, N == N1],
    PhaseGroup = [X || #action{ap = N1} = X <- KeyList, N == N1],
    case PhaseGroup of
        [] ->
            group_phases(N - 1, KeyList, Acc);
        _ ->
            group_phases(N - 1, KeyList, [PhaseGroup | Acc])
    end.
