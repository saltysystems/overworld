%%%--------------------------------------------------------------------
%%% @copyright (C) 2021, Salty Systems
%%% @doc Goblet Game Module. Provides game logic.
%%% @end
%%%--------------------------------------------------------------------
-module(goblet_game).

-export([
    new_player/4,
    initialize_board/3
]).

%======================================================================
% Public API
%======================================================================

-spec new_player(list(), pos_integer(), atom(), list()) ->
    ok | {error, any()}.
new_player(Name, Appearance, Role, Account) ->
    case
        goblet_util:run_checks([
            fun() -> is_valid_name(Name) end,
            fun() -> is_valid_appearance(Appearance) end,
            fun() -> is_valid_role(Role) end
        ])
    of
        ok ->
            goblet_db:create_player(Name, Appearance, Role, Account),
            % Now give the player some basic inventory
            % TODO: replace me with a more dynamic system
            goblet_db:item_to_player("Reactor MK I", Name),
            goblet_db:item_to_player("Missile MK I", Name),
            goblet_db:item_to_player("Beam MK I", Name),
            goblet_db:item_to_player("Repair MK I", Name),
            goblet_db:item_to_player("Scanner MK I", Name);
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

%======================================================================
% Internal Functions
%======================================================================
is_valid_name(Name) when length(Name) < 64 ->
    ok;
is_valid_name(_Name) ->
    too_long.

% should be a positive integer to be a valid protobuf message
% TODO: Fix the randomly chosen value of 10!
is_valid_appearance(Appearance) when Appearance < 10 ->
    ok;
is_valid_appearance(_Appearance) ->
    no_such_appearance.

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
