%%%--------------------------------------------------------------------
%%% @copyright (C) 2021, Salty Systems
%%% @doc Goblet Game Module. Provides game logic.
%%% @end
%%%--------------------------------------------------------------------
-module(goblet_game).

-export([
    new_player/5,
    initialize_board/3,
    initialize_mobs/2,
    calculate_round/1,
    normalize_actions/1,
    phases/1,
    check_valid_items/2,
    check_player_alive/1,
    check_valid_target/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(action, {type, ap, who, target}).
-record(gamestate, {mobs, players, actions, board}).
%TODO - maybe use a record
%-record(entity, {name, health, energy, flags, inventory}).

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
%TODO: Add mob initialization

initialize_board(Board, []) ->
    Board;
initialize_board(Board, [Player | Rest]) ->
    Coords = goblet_board:get_first_unoccupied_tile(Board),
    case goblet_board:add_pawn(Player, Coords, Board) of
        {error, Board} ->
            Board;
        B1 ->
            initialize_board(B1, Rest)
    end.

initialize_mobs(Board, [Mob | Rest]) ->
    Coords = goblet_board:get_random_unoccupied_tile(Board),
    case goblet_board:add_pawn(Mob, Coords, Board) of
        {error, Board} ->
            Board;
        B1 ->
            initialize_board(B1, Rest)
    end.

calculate_round({Actions, Mobs, Players, Board}) ->
    % TODO: Need to run the pipeline for EACH moment in the round.
    G0 = #gamestate{
        actions = Actions,
        mobs = Mobs,
        players = Players,
        board = Board
    },
    % Clean up dead players at the beginning of the round as a maintenance
    % task of sorts - the clients should have seen Health go to 0 or less
    % and played a destruction animation appropriately.
    GN = goblet_util:pipeline(G0, [
        fun(S) -> cleanup_dead(S) end,
        fun(S) -> cleanup_dead(S) end,
        fun(S) -> update_status_effects(S) end,
        fun(S) -> regenerate_energy(S) end,
        fun(S) -> process_actions(S) end
    ]),
    NewPlayerNames = update_players(GN),
    % Then return the updated gamestate to the caller
    {[], GN#gamestate.mobs, NewPlayerNames, GN#gamestate.board}.

cleanup_dead(S) ->
    Mobs = S#gamestate.mobs,
    Players = S#gamestate.players,
    DeadMobs = cleanup_dead(S#gamestate.mobs, []),
    DeadPlayers = cleanup_dead(S#gamestate.players, []),
    S#gamestate{mobs = Mobs -- DeadMobs, players = Players -- DeadPlayers}.

cleanup_dead([], Acc) ->
    Acc;
cleanup_dead([{_N, Health, _E, _F, _I} = H | T], Acc) when Health =< 0 ->
    cleanup_dead(T, [H | Acc]);
cleanup_dead([_H | T], Acc) ->
    cleanup_dead(T, Acc).

update_status_effects(S) ->
    % stubby stub
    S.

regenerate_energy(S) ->
    Mobs = regenerate_energy(S#gamestate.mobs, []),
    Players = regenerate_energy(S#gamestate.players, []),
    S#gamestate{mobs = Mobs, players = Players}.

regenerate_energy([], Acc) ->
    Acc;
regenerate_energy([{_N, _H, E, _F, _I} = H | T], Acc) when E >= 100 ->
    regenerate_energy(T, [H | Acc]);
regenerate_energy([{N, H, Energy, F, I} | T], Acc) ->
    % Cap energy at 100
    E =
        if
            Energy + 10 > 100 ->
                100;
            true ->
                Energy
        end,
    regenerate_energy(T, [{N, H, E, F, I} | Acc]).

% the big kahuna
process_actions(S) ->
    Actions = process_action_list(S#gamestate.actions),
    Phases = phases(lists:flatten(Actions)),
    % Once we have phases, start processing them and build the "playlist" for
    % the client
    process_phases(Phases).

process_phases(_Phases) ->
    ok.
%TODO: Process the phase list, which is a list of lists.
%      Need to process each phase, calculate damage/heals/status
%      then build the result list for the clients.

process_action_list(List) ->
    process_action_list(List, []).
process_action_list([], Acc) ->
    Acc;
process_action_list([PlayersActions | Rest], Acc) ->
    % For each player in the list, convert the actions to a record
    R = make_action_record(PlayersActions),
    % Then normalize the actions
    N = normalize_actions(R),
    % Then call recursively with the rest
    process_action_list(Rest, [N | Acc]).

make_action_record(Actions) ->
    make_action_record(Actions, []).
make_action_record([], Acc) ->
    Acc;
make_action_record([{Player, Item, Target} | Rest], Acc) ->
    {Type, AP} = goblet_db:item_type_and_ap(Item),
    Action = #action{type = Type, ap = AP, who = Player, target = Target},
    make_action_record(Rest, [Action | Acc]).

update_players(#gamestate{players = Players}) ->
    % commit each player's updates to the database
    Results = [
        {Name, goblet_db:player_unshadow(Name, Health, Energy, Flags)}
     || {Name, Health, Energy, Flags, _I} <- Players
    ],
    % Then return the names of the players
    % (note this effectively throws away the result of the side effects.
    % might want to check for errors in the future.)
    [Name || {Name, _} <- Results].

check_valid_items([], _Inventory) ->
    ok;
check_valid_items([{Player, Item, {_X, _Y}} | T], Inventory) ->
    Inv = goblet_db:player_inventory(Player),
    case lists:member(Item, Inv) of
        false ->
            {error, invalid_action};
        _ ->
            check_valid_items(T, Inventory)
    end.

%check_valid_actions([]) ->
%    ok;
%check_valid_actions([{Player, Type, {_X, _Y}} | T]) ->
%    case goblet_db:player_items_have_action(Player, Type) of
%        [] ->
%            {error, invalid_action};
%        {error, E} ->
%            {error, E};
%        _ ->
%            check_valid_actions(T)
%    end.

check_valid_target([], _MatchID) ->
    ok;
check_valid_target([{_Player, _Type, {X, Y}} | T], MatchID) ->
    % what is a valid target? can't go out of bounds, for one.
    B = goblet_instance:get_board_state(MatchID),
    {MaxX, MaxY} = goblet_board:get_last_tile(B),
    case within_bounds(X, MaxX, Y, MaxY) of
        {error, E} ->
            {error, E};
        _ ->
            check_valid_target(T, MatchID)
    end.

within_bounds(X, MaxX, Y, MaxY) when 0 > X; X > MaxX; 0 > Y; Y > MaxY ->
    {error, out_of_bounds};
within_bounds(_X, _MaxX, _Y, _MaxY) ->
    ok.

check_player_alive(Player) ->
    case goblet_db:is_player_alive(Player) of
        true -> ok;
        false -> {error, player_dead};
        {error, E} -> {error, E}
    end.

normalize_actions(ActionList) ->
    % First we need to split the list by Who committed the action
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
    % AP must be the second item in the record, hence 3rd key
    S = lists:keysort(3, ActionList),
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
        #action{type = shoot, ap = 2, who = "player1", target = "player2"},
        #action{type = move, ap = 1, who = "player1", target = {0, 0}},
        #action{type = laz0r, ap = 6, who = "player2", target = "player1"}
    ],
    % Convert AP to Phases
    MP = normalize_actions(Actions),
    Last = lists:last(MP),
    ?assertEqual(9, Last#action.ap).

phases_test() ->
    P1_Actions = [
        #action{type = shoot, ap = 2, who = "player1", target = "player2"},
        #action{type = move, ap = 1, who = "player1", target = {0, 0}},
        #action{type = laz0r, ap = 6, who = "player1", target = "player2"}
    ],
    P2_Actions = [
        #action{type = move, ap = 1, who = "player2", target = {1, 1}},
        #action{type = shoot, ap = 2, who = "player2", target = "player1"},
        #action{
            type = shield,
            ap = 3,
            who = "player2",
            target = "player1"
        },
        #action{
            type = missile,
            ap = 3,
            who = "player2",
            target = "player1"
        }
    ],
    % Convert AP to Phases
    MP = normalize_actions(P1_Actions),
    MP2 = normalize_actions(P2_Actions),
    % Group the phases
    [P1, P2, P3, P4, P5] = phases(MP ++ MP2),
    ?assertEqual(1, length(P1)),
    ?assertEqual(1, length(P2)),
    ?assertEqual(2, length(P3)),
    ?assertEqual(1, length(P4)),
    ?assertEqual(2, length(P5)).

process_action_list_test() ->
    % Items from the db..
    % "Missile MK I', "Repair MK I", "Beam MK I", "Scanner MK I",
    % "Reactor MK I",
    % First create the list of actions as it comes from the instance server.
    P1 = [
        {"Chester", "Reactor MK I", {3, 4}},
        {"Chester", "Beam MK I", {1, 2}}
    ],
    P2 = [{"Tad", "Beam MK I", {3, 4}}, {"Tad", "Repair MK I", "Chester"}],
    P3 = [
        {"Ralph", "Scanner MK I", {1, 1}},
        {"Ralph", "Scanner MK I", {1, 2}}
    ],
    R = process_action_list([P1, P2, P3]),
    ?assertEqual(length(R), 3),
    % Now process into phases
    [R1, R2, R3, R4, R5] = phases(lists:flatten(R)),
    ?assertEqual(1, length(R1)),
    ?assertEqual(2, length(R2)),
    ?assertEqual(1, length(R3)),
    ?assertEqual(1, length(R4)),
    ?assertEqual(1, length(R5)).

within_bounds_ok_test() ->
    X = 3,
    MaxX = 3,
    Y = 3,
    MaxY = 3,
    R = within_bounds(X, MaxX, Y, MaxY),
    ?assertEqual(ok, R).

within_bounds_oobX_test() ->
    X = 3,
    MaxX = 2,
    Y = 3,
    MaxY = 3,
    R = within_bounds(X, MaxX, Y, MaxY),
    ?assertEqual({error, out_of_bounds}, R).

within_bounds_oobY_test() ->
    X = 3,
    MaxX = 3,
    Y = 3,
    MaxY = 2,
    R = within_bounds(X, MaxX, Y, MaxY),
    ?assertEqual({error, out_of_bounds}, R).

within_bounds_negative_test() ->
    X = -1,
    MaxX = 3,
    Y = 3,
    MaxY = 3,
    R = within_bounds(X, MaxX, Y, MaxY),
    ?assertEqual({error, out_of_bounds}, R).
