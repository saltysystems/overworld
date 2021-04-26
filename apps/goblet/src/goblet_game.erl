%%%--------------------------------------------------------------------
%%% @copyright (C) 2021, Salty Systems
%%% @doc Goblet Game Module. Provides game logic.
%%% @end
%%%--------------------------------------------------------------------
-module(goblet_game).

-export([
    new_player/5,
    initialize_board/3
]).

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
