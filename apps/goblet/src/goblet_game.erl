%%%--------------------------------------------------------------------
%%% @copyright (C) 2021, Salty Systems
%%% @doc Goblet Game Module. Provides game logic.
%%% @end
%%%--------------------------------------------------------------------
-module(goblet_game).

-export([
    new_player/5,
    initialize_board/3,
    calculate_round/2,
    normalize_actions/1,
    check_valid_actions/1,
    check_player_alive/1
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(action, {ap, name, from, target}).

%======================================================================
% Public API
%======================================================================

-spec new_player(list(), list(), list(), atom(), list()) ->
    ok | {error, any()}.
new_player(Name, Colors, Symbols, Role, Account) ->
    case
        goblet_util:run_checks([
            fun() -> is_valid_name(Name) end,
            fun() -> is_valid_appearance(Symbols, Colors) end,
            fun() -> is_valid_role(Role) end
        ])
    of
        ok ->
            goblet_db:create_player(Name, Colors, Symbols, Role, Account),
            % Now give the player some basic inventory
            % TODO: replace me with a more dynamic system
            goblet_db:item_to_player("Reactor MK I", Name);
        Error ->
            {error, Error}
    end.

-spec initialize_board(pos_integer(), pos_integer(), list()) -> list().
initialize_board(X, Y, Players) ->
    Board = goblet_board:new(X, Y),
    initialize_board(Board, Players).

initialize_board(Board, []) ->
    Board;
initialize_board(Board, [Player | Rest]) ->
    Coords = goblet_board:get_unoccupied_tile(Board),
    case goblet_board:add_pawn(Player, Coords, Board) of
        {error, Board} ->
            Board;
        B1 ->
            initialize_board(B1, Rest)
    end.

calculate_round(_Actions, Board) ->
    Board.

check_valid_actions([]) ->
    ok;
check_valid_actions([{Player, Type, {X,Y}} | T]) ->
    case goblet_db:player_items_have_action(Player, Type) of
        [] ->
            {error, invalid_action};
        {error, E} ->
            {error, E};
        _ ->
            check_valid_actions(T)
    end.

check_valid_target([{Player, Type, {X,Y}} | T], MatchID) ->
    B = goblet_instance:get_board_state(MatchID).

check_player_alive(Player) ->
    case goblet_db:is_player_alive(Player) of
        true -> ok;
        false -> {error, player_dead};
        {error, E} -> {error, E}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Internal Functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    % Recurse through the phases, pairing up matching keys into the same
    % phase til we hit 0.
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

is_valid_name(Name) when length(Name) < 64 ->
    case goblet_db:player_by_name(Name) of
        {error, no_such_player} -> ok;
        _Result -> already_exists
    end;
is_valid_name(_Name) ->
    name_too_long.

% should be a positive integer to be a valid protobuf message
is_valid_appearance(_Symbols, _Colors) ->
    % Easter egg: You can hack up the client to send any color and symbol
    % combination. If you wanted to prevent that, this would be the place.
    ok.

is_valid_role(Role) ->
    ValidRoles = ['DESTROYER', 'INTERCEPTOR', 'CARRIER', 'COMMAND'],
    case lists:member(Role, ValidRoles) of
        true ->
            ok;
        false ->
            not_a_class
    end.

%======================================================================
% Tests
%======================================================================
normalize_actions_test() ->
    Actions = [
        #action{name = shoot, ap = 2, from = "player1", target = "player2"},
        #action{name = move, ap = 1, from = "player1", target = {0, 0}},
        #action{name = laz0r, ap = 6, from = "player2", target = "player1"}
    ],
    % Convert AP to Phases
    MP = normalize_actions(Actions),
    Last = lists:last(MP),
    ?assertEqual(9, Last#action.ap).

phases_test() ->
    P1_Actions = [
        #action{name = shoot, ap = 2, from = "player1", target = "player2"},
        #action{name = move, ap = 1, from = "player1", target = {0, 0}},
        #action{name = laz0r, ap = 6, from = "player1", target = "player2"}
    ],
    P2_Actions = [
        #action{name = move, ap = 1, from = "player2", target = {1, 1}},
        #action{name = shoot, ap = 2, from = "player2", target = "player1"},
        #action{
            name = shield,
            ap = 3,
            from = "player2",
            target = "player1"
        },
        #action{
            name = missile,
            ap = 3,
            from = "player2",
            target = "player1"
        }
    ],
    % Convert AP to Phases
    MP = normalize_actions(P1_Actions),
    MP2 = normalize_actions(P2_Actions),
    Collected = MP ++ MP2,
    % Group the phases
    phases(Collected).
