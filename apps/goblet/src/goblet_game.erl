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
    check_valid_items/2,
    check_player_alive/1,
    check_valid_target/2,
    maybe_notify_intent/2,
    deduct_energy/3
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(action, {
    type,
    ap,
    who,
    target,
    visible
}).

-record(gamestate, {
    mobs,
    players,
    actions,
    board,
    replay = []
}).

%-type action() :: #action{}.
%-export_type([action/4]).
%-type gamestate() :: #gamestate{}.

%TODO - maybe use a record
%-record(entity, {name, health, energy, flags, inventory}).

%======================================================================
% Public API
%======================================================================

%-spec new_player(list(), list(), list(), atom(), list()) ->
%    ok | {error, any()}.
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

%-spec initialize_board(pos_integer(), pos_integer(), list()) -> list().
initialize_board(X, Y, Players) ->
    Board0 = goblet_board:new(X, Y),
    Board1 = initialize_board(Board0, Players),
    initialize_mobs(Board1).

%-spec initialize_board(list(), list()) -> list().
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

initialize_board_test() ->
    B = initialize_board(1, 1, ["Test"]),
    {tile, {X, Y}, Type, _Flags, Name, _VisibleTo} = lists:keyfind(
        "Test",
        5,
        B
    ),
    ?assertEqual(true, is_integer(X)),
    ?assertEqual(true, is_integer(Y)),
    ?assertEqual(f, Type),
    ?assertEqual("Test", Name).

%-spec initialize_mobs(list(), list()) -> list().
initialize_mobs(Board) ->
    Board.

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
        fun(S) -> update_status_effects(S) end,
        fun(S) -> regenerate_energy(S) end,
        fun(S) -> process_actions(S) end
    ]),
    NewPlayerNames = update_players(GN),
    % Then return the updated gamestate to the caller
    {
        [],
        GN#gamestate.mobs,
        NewPlayerNames,
        GN#gamestate.board,
        GN#gamestate.replay
    }.

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

cleanup_dead_test() ->
    P0 = [
        {"Chester", 100, 100, [], []},
        {"Leser", -10, 100, [], []},
        {a_mob_01, 0, 10, [], []}
    ],
    Deads = cleanup_dead(P0, []),
    [{Name, _, _, _, _}] = P0 -- Deads,
    ?assertEqual("Chester", Name).

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
    %TODO Replace this with an item
    % Cap energy at 100
    E =
        if
            Energy + 10 > 100 ->
                100;
            true ->
                Energy + 10
        end,
    regenerate_energy(T, [{N, H, E, F, I} | Acc]).

regenerate_energy_test() ->
    P0 = [
        {"Chester", 100, 100, [], []},
        {a_mob_01, 40, 10, [], []}
    ],
    [H | T] = lists:reverse(regenerate_energy(P0, [])),
    {_, _, E0, _, _} = H,
    [{_, _, E1, _, _}] = T,
    ?assertEqual(100, E0),
    ?assertEqual(20, E1).

% the big kahuna
process_actions(S) ->
    Actions = process_action_list(S#gamestate.actions),
    Phases = phases(lists:flatten(Actions)),
    % Once we have phases, start processing them and build the "playlist" for
    % the client
    % Can also clear the actions since we broke it out into the phase list
    % so there's no need to pass around extraneous data
    process_phases(Phases, S#gamestate{actions = []}).

process_phases([], Gamestate) ->
    % Reverse the replay list to match expectations
    R0 = Gamestate#gamestate.replay,
    Gamestate#gamestate{replay = lists:reverse(R0)};
process_phases([H | T], Gamestate) ->
    % Process one phase
    G1 = update_gamestate(H, Gamestate),
    % Process the rest of the phases
    process_phases(T, G1).

update_gamestate([], G) ->
    G;
update_gamestate([Action | T], G) ->
    %Board = G#gamestate.board,
    %Coords = goblet_board:get_pawn(Player, Board),
    Who = Action#action.who,
    % temporary
    Cost = Action#action.ap * 10,
    PlayerList = G#gamestate.players,
    GN =
        case deduct_energy(Who, Cost, PlayerList) of
            {ok, UpdatedPlayerList} ->
                % New Gamestate with the player energy reduced appropriately
                G1 = G#gamestate{players = UpdatedPlayerList},
                action_processor(Action, G1);
            {Error, _PlayerList} ->
                logger:warning(
                    "Something went wrong deducting energy from ~p: ~p",
                    [Who, Error]
                ),
                G
        end,
    update_gamestate(T, GN).

action_processor(Action, CurrentGamestate) ->
    Player = Action#action.who,
    Type = Action#action.type,
    Target = Action#action.target,
    Board = CurrentGamestate#gamestate.board,
    Coords = goblet_board:get_pawn(Player, Board),
    CurrentReplay = CurrentGamestate#gamestate.replay,
    NewGamestate =
        case Type of
            move ->
                NewBoard = move_maybe_collide(Coords, Target, Board),
                NewReplay = record_replay(
                    Player,
                    atom_to_list(Type),
                    Target,
                    CurrentReplay
                ),
                CurrentGamestate#gamestate{
                    board = NewBoard,
                    replay = NewReplay
                };
            _ ->
                % Do nothing if the action type isn't recognized.
                logger:warning(
                    "Action '~p' not understood. Cowardly doing nothing",
                    [Type]
                ),
                CurrentGamestate
        end,
    NewGamestate.

%TODO: Think about some kind of meta language to describe this, maybe also a
%      place where gdminus could fit in. Perhaps something like how Chess moves
%      are described?
-spec record_replay(list(), list(), tuple(), list()) -> list().
record_replay(Player, Type, Target, CurrentReplay) ->
    Replay = {Player, Type, Target},
    [Replay | CurrentReplay].

-spec move_maybe_collide(tuple(), tuple(), list()) -> list().
move_maybe_collide(From, To, Board) ->
	GridDistance = goblet_board:grid_distance(From, To),
	move_maybe_collide(From, To, Board, GridDistance).

move_maybe_collide(From, To, Board, GridDistance) when GridDistance =< 1 ->
    FromOcc = goblet_board:get_tile_occupant(From, Board),
    ToOcc = goblet_board:get_tile_occupant(To, Board),
    case FromOcc of
        [] ->
            move_pawn(From, To, Board);
        FromOcc ->
            if
                FromOcc == ToOcc ->
                    logger:warning(
                        "Occupant is the same in both tiles, doing nothing"
                    ),
                    Board;
                true ->
                    % just for clarity
                    OccupantPos = To,
                    NewPos = goblet_board:get_nearest_unoccupied_tile(
                        OccupantPos,
                        Board
                    ),
                    NewBoard = move_pawn(OccupantPos, NewPos, Board),
                    % Now move the original player to their intended position
                    move_pawn(From, To, NewBoard)
            end
    end;
move_maybe_collide(_From, _To, Board, _GridDistance) ->
	Board.

move_maybe_collide_test() ->
    B1 = goblet_board:new(1, 1),
    P1 = "Chester",
    PlayerOnePos = goblet_board:get_first_unoccupied_tile(B1),
    B2 = goblet_board:add_pawn(P1, PlayerOnePos, B1),
    P2 = "Lester",
    PlayerTwoPos = goblet_board:get_first_unoccupied_tile(B2),
    B3 = goblet_board:add_pawn(P2, PlayerTwoPos, B2),
    B4 = move_maybe_collide(PlayerTwoPos, PlayerOnePos, B3),
    % Player one should now be bounced to a new position, and P2 moves into
    % P1's place
    NewPlayerOnePos = goblet_board:get_pawn(P1, B4),
    NewPlayerTwoPos = goblet_board:get_pawn(P2, B4),
    ?assertNotEqual(NewPlayerOnePos, PlayerOnePos),
    %While Player 2 should now be in Player 1's old position
    ?assertEqual(NewPlayerTwoPos, PlayerOnePos).

-spec move_pawn(tuple(), tuple(), list()) -> list().
move_pawn(From, To, Board) ->
    case goblet_board:mv_pawn(From, To, Board) of
        {ok, Board1} ->
            Board1;
        {error, Board} ->
            logger:warning(
                "Player attempted illegal move to board position ~p",
                [To]
            ),
            Board
    end.

update_gamestate_test() ->
    P0 = [
        {"Chester", 100, 100, [], []}
    ],
    M0 = [
        {a_mob_01, 40, 10, [], []}
    ],
    A0 = #action{type = move, ap = 3, who = "Chester", target = {1, 3}},
    B0 = initialize_board(1, 1, ["Chester"]),
    % mobs, players, actions, board
    G0 = #gamestate{mobs = M0, players = P0, actions = A0, board = B0},
    G1 = update_gamestate([A0], G0),
    Location = goblet_board:get_pawn("Chester", G1#gamestate.board),
    ?assertEqual({1, 3}, Location).

deduct_energy(Name, Cost, PlayerList) ->
    % If we can't find the player we should probably just crash.
    {N, H, E, F, I} = lists:keyfind(Name, 1, PlayerList),
    E1 = E - Cost,
    if
        E1 < 0 ->
            {no_energy, PlayerList};
        true ->
            UpdatedPlayerList = lists:keyreplace(
                Name,
                1,
                PlayerList,
                {N, H, E - Cost, F, I}
            ),
            {ok, UpdatedPlayerList}
    end.

% trying out putting the tests with the funs otherwise I have to jump around
deduct_energy_test() ->
    PlayerList = [
        {"Chester", 100, 100, [], []},
        {"Lester", 120, 90, [], []}
    ],
    Who = "Chester",
    {Resp0, [H0 | _T0]} = deduct_energy(Who, 30, PlayerList),
    ?assertEqual(Resp0, ok),
    ?assertEqual({"Chester", 100, 70, [], []}, H0),
    % Now test over-deducting
    {Resp1, [H1 | _T1]} = deduct_energy(Who, 120, PlayerList),
    ?assertEqual(Resp1, no_energy),
    ?assertEqual({"Chester", 100, 100, [], []}, H1).

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
    {Type, AP, Flags} = goblet_db:item_to_action(Item),
    Visible = lists:member("visible", Flags),
    Action = #action{
        type = Type,
        ap = AP,
        who = Player,
        target = Target,
        visible = Visible
    },
    make_action_record(Rest, [Action | Acc]).

update_players(S) ->
    Players = S#gamestate.players,
    % commit each player's updates to the database
    Results = [
        {Name,
            goblet_db:player_unshadow(
                Name,
                Health,
                Energy,
                Flags,
                Inventory
            )}
        || {Name, Health, Energy, Flags, Inventory} <- Players
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

normalize_actions([]) ->
    [];
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
    normalize_actions(T, [MPList | [AccH | AccT]]).

% The ActionList must already do the math needed to handle this
phases([]) ->
    [];
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

-spec is_valid_name(list()) -> atom().
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

-spec is_valid_role(atom()) -> atom().
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
    % "Missile MK I", "Repair MK I", "Beam MK I", "Scanner MK I",
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
    ?assertEqual(length(R), 3).

action_list_phase_test() ->
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

% This function is called primarily for its side effects to notify users in a
% match of something happening
maybe_notify_intent([], _ID) ->
    ok;
maybe_notify_intent([{Who, Item, Target} | T], ID) ->
    {Action, _AP, Flags} = goblet_db:item_to_action(Item),
    case lists:member(visible, Flags) of
        true ->
            logger:notice("Notifying match ~p of ~p's action ~p", [
                ID,
                Who,
                Action
            ]),
            goblet_protocol:match_broadcast_intent(
                Who,
                atom_to_list(Action),
                Target,
                ID
            );
        false ->
            ok
    end,
    maybe_notify_intent(T, ID).
