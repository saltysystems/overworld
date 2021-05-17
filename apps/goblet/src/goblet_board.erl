%%%-------------------------------------------------------------------------
%%% @copyright (C) 2021, Salty Systems
%%% @doc Goblet board representation. This module will provide primitives
%%%      for adding, moving, removing pawns on tiles. It will also provide
%%%      low-level functions to reveal and hide tiles, or get the state of
%%%      a particular tile.
%%% @end
%%%-------------------------------------------------------------------------
-module(goblet_board).

-export([
    new/2,
    add_pawn/3,
    rm_pawn/2,
    mv_pawn/3,
    get_pawn/2,
    reveal_tile/3,
    hide_tile/3,
    get_tile_all/2,
    get_tile_occupant/2,
    get_tile_reachable/4,
    get_first_unoccupied_tile/1,
    get_random_unoccupied_tile/1,
    get_last_tile/1
]).

-include_lib("eunit/include/eunit.hrl").

-type tile_coords() :: {integer(), integer()}.
-type tile_type() :: w | f.

-record(tile, {
    coordinates :: tile_coords(),
    type :: tile_type(),
    flags = [],
    occupant = [],
    visible_to = []
}).

% Some constants
-define(SHIP_MAX_RANGE, 5).
-define(WALL, w).
-define(FLOOR, f).
-define(FLOOD, 'F').
-define(SECTOR_15, {
    {w, f, f, f, w},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {w, f, f, f, w}
}).
-define(SECTOR_14, {
    {w, w, w, w, w},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {w, f, f, f, w}
}).
-define(SECTOR_13, {
    {w, f, f, f, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {w, f, f, f, w}
}).
-define(SECTOR_12, {
    {w, w, w, w, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {w, f, f, f, w}
}).
-define(SECTOR_11, {
    {w, f, f, f, w},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {w, w, w, w, w}
}).
-define(SECTOR_10, {
    {w, w, w, w, w},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {f, f, f, f, f},
    {w, w, w, w, w}
}).
-define(SECTOR_9, {
    {w, f, f, f, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {f, f, f, f, w},
    {w, w, w, w, w}
}).
-define(SECTOR_7, {
    {w, f, f, f, w},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, f, f, f, w}
}).
-define(SECTOR_6, {
    {w, w, w, w, w},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, f, f, f, w}
}).
-define(SECTOR_5, {
    {w, f, f, f, w},
    {w, f, f, f, w},
    {w, f, f, f, w},
    {w, f, f, f, w},
    {w, f, f, f, w}
}).
-define(SECTOR_3, {
    {w, f, f, f, w},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, f, f, f, f},
    {w, w, w, w, w}
}).

%---------------------------------------------------------------------------
% @doc Initializes the state of the board and returns a map containing the
%      sector list and tile list. The former defines the high level
%      structure and hints to the client how to draw the scene, while the
%      latter holds the state of individual tiles.
% @end
%---------------------------------------------------------------------------
-spec new(pos_integer(), pos_integer()) -> [{_, _, _, _, _, _}].
new(X, Y) when X > 0, Y > 0 ->
    %TODO: This is ugly necessary magic due to the macros above
    TilesPerSector = {5, 5},
    SectorList = newMap(X - 1, Y - 1),
    TileList = repack(SectorList, TilesPerSector),

    % choose the first valid floor tile, then attempt a floodfill
    InitialTiles = lists:keyfind(?FLOOR, #tile.type, TileList),
    {Xi, Yi} = InitialTiles#tile.coordinates,
    Flood = floodfill(TileList, Xi, Yi),
    % check to see if the whole board was flooded. if so, use the board
    % generated. if not, re-run the generation task.
    case check_flooded_tiles(Flood) of
        true -> TileList;
        _ -> new(X, Y)
    end;
new(_X, _Y) ->
    [].

%---------------------------------------------------------------------------
% @doc Adds a pawn to a tile at the indicated coordinates in a given list.
%      Explicitly checks for walls, and whether or not the tile is already
%      occupied. Returns a new list with the updated state. Errors are
%      indicated on a best effort basis. If the coordinates do not exist,
%      the function returns successfully but there will be no update.
% @end
%---------------------------------------------------------------------------
-spec add_pawn(any(), {pos_integer(), pos_integer()}, list()) ->
    list() | {atom(), list()}.
% tile is a wall, can't add a goon
add_pawn(_Pawn, _Coordinates, TileList) when TileList#tile.type =:= ?WALL ->
    {error, TileList};
add_pawn(Pawn, Coordinates, TileList) ->
    Tile = get_tile(Coordinates, TileList),
    case Tile#tile.occupant of
        [] ->
            NewTile = Tile#tile{occupant = Pawn},
            NewTileList = lists:keyreplace(
                Coordinates,
                #tile.coordinates,
                TileList,
                NewTile
            ),
            NewTileList;
        _ ->
            {error, TileList}
    end.

%---------------------------------------------------------------------------
% @doc Removes a pawn from a tile list at the indicated coordinates.
%      Returns a list with the updated state. Will always return
%      successfully, but will not validate the state before removing a
%      pawn. Removing a pawn from an already empty tile is effectively a
%      no-op.
% @end
%---------------------------------------------------------------------------
-spec rm_pawn({non_neg_integer(), non_neg_integer()}, list()) ->
    {atom(), list()}.
rm_pawn(Coordinates, TileList) ->
    Tile = get_tile(Coordinates, TileList),
    NewTile = Tile#tile{occupant = []},
    NewTileList = update_tile(Coordinates, NewTile, TileList),
    {ok, NewTileList}.

%---------------------------------------------------------------------------
% @doc Moves any occupant of the first argument to the position of the
%      second argument. Returns an updated list.
% @end
%---------------------------------------------------------------------------
-spec mv_pawn(
    {non_neg_integer(), non_neg_integer()},
    {non_neg_integer(), non_neg_integer()},
    list()
) -> {atom(), list()}.
mv_pawn(CoordinatesFrom, CoordinatesTo, TileList) ->
    TileFrom = get_tile(CoordinatesFrom, TileList),
    TileTo = get_tile(CoordinatesTo, TileList),
    CanMove = goblet_util:run_checks([
        fun() -> is_tile(not_a_wall, TileTo) end
    ]),
    %CanMove = util:run_checks(CheckList),
    case CanMove of
        ok ->
            Pawn = TileFrom#tile.occupant,
            NewTileF = TileFrom#tile{occupant = []},
            NewTileList = update_tile(CoordinatesFrom, NewTileF, TileList),
            NewTileT = TileTo#tile{occupant = Pawn},
            {ok, update_tile(CoordinatesTo, NewTileT, NewTileList)};
        _ ->
            {error, TileList}
    end.

%---------------------------------------------------------------------------
% @doc Moves any occupant of the first argument to the position of the
%      second argument. Returns an updated list.
% @end
%---------------------------------------------------------------------------
-spec get_pawn(list(), list()) -> tile_coords().
get_pawn(Name, TileList) ->
    Tile = lists:keyfind(Name, #tile.occupant, TileList),
    Tile#tile.coordinates.

%---------------------------------------------------------------------------
% @doc Adds one unique instance of a playerID to this tile. Does not
%      guarantee order. Does not allow duplicates and will actively purge
%      duplicates.  Always returns ok with the updated tile.
% @end
%---------------------------------------------------------------------------
-spec reveal_tile(any(), {pos_integer(), pos_integer()}, list()) ->
    {ok, list()}.
reveal_tile(PlayerID, Coordinates, TileList) ->
    Tile = get_tile(Coordinates, TileList),
    VisibleTo = Tile#tile.visible_to,
    VisibleToSet = sets:from_list(VisibleTo),
    NewSet = sets:add_element(PlayerID, VisibleToSet),
    {ok,
        update_tile(
            Coordinates,
            Tile#tile{visible_to = sets:to_list(NewSet)},
            TileList
        )}.

%---------------------------------------------------------------------------
% @doc Removes the first (and hopefully only) instance some identifier
%      (such as Player ID) from the visibility list of the specified tile.
%      Returns an updated copy of the TileList. Will safely do nothing with
%      a player for whom the tile that is already hidden.
% @end
%---------------------------------------------------------------------------
-spec hide_tile(any(), {pos_integer(), pos_integer()}, list()) ->
    {atom(), list()}.
hide_tile(PlayerID, Coordinates, TileList) ->
    Tile = get_tile(Coordinates, TileList),
    VisibleTo = Tile#tile.visible_to,
    NewVisible = lists:delete(PlayerID, VisibleTo),
    {ok,
        update_tile(
            Coordinates,
            Tile#tile{visible_to = NewVisible},
            TileList
        )}.

%----------------------------------------------------------------------
% @doc Dump the entire tile state
% @end
%----------------------------------------------------------------------
-spec get_tile_all({non_neg_integer(), non_neg_integer()}, list()) ->
    tuple().
get_tile_all({X, Y}, TileList) ->
    Tile = get_tile({X, Y}, TileList),
    {X, Y} = Tile#tile.coordinates,
    {X, Y, Tile#tile.occupant, Tile#tile.type, Tile#tile.flags}.

%----------------------------------------------------------------------
% @doc Get the occupant of a tile
% @end
%----------------------------------------------------------------------
-spec get_tile_occupant({integer(), integer()}, list()) -> list().
get_tile_occupant({X, Y}, TileList) ->
    Tile = get_tile({X, Y}, TileList),
    Tile#tile.occupant.

%----------------------------------------------------------------------
% @doc Check whether or not a tile is reachable
%----------------------------------------------------------------------
get_tile_reachable(T1, T2, Board, MaxDistance) ->
    CanMove = goblet_util:run_checks([
        fun() -> is_tile(not_a_wall, T1) end,
        fun() -> is_tile(not_a_wall, T2) end,
        fun() -> is_tile(empty, T2) end
    ]),
    get_tile_reachable(T1, T2, Board, MaxDistance, CanMove).
get_tile_reachable(T1, T2, _Board, MaxDistance, ok) ->
    {X1, Y1} = T1,
    {X2, Y2} = T2,
    max(abs(Y2 - Y1), abs(X2 - X1)) =< MaxDistance;
get_tile_reachable(_T1, _T2, _Board, _MovementFun, _CanMove) ->
    false.

%----------------------------------------------------------------------
% @doc Get the first floor tile which has no occupant. Recurse through the
%      list until an unoccupied tile can be found. Crash horribly if there
%      are no free tiles.
% @end
%----------------------------------------------------------------------
-spec get_first_unoccupied_tile(list()) -> tuple().
get_first_unoccupied_tile(TileList) ->
    {_, Tile, NewTileList} = lists:keytake(?FLOOR, #tile.type, TileList),
    case Tile#tile.occupant of
        [] ->
            Tile#tile.coordinates;
        _ ->
            get_first_unoccupied_tile(NewTileList)
    end.

%----------------------------------------------------------------------
% @doc Get a random floor tile who has no occupant. Recurse through the list
%      until an unoccupied tile can be found. Crash horribly if there are no
%      free tiles.
% @end
%----------------------------------------------------------------------
-spec get_random_unoccupied_tile(list()) -> tuple().
get_random_unoccupied_tile(TileList) ->
    % Sort the tile list randomly. Stack overflo like a pro
    RList = [
        X
     || {_, X} <- lists:sort([{rand:uniform(), N} || N <- TileList])
    ],
    {_, Tile, NewTileList} = lists:keytake(?FLOOR, #tile.type, RList),
    case Tile#tile.occupant of
        [] ->
            Tile#tile.coordinates;
        _ ->
            % No need to sort the list another time
            get_first_unoccupied_tile(NewTileList)
    end.

%----------------------------------------------------------------------
% @doc Get the largest floor tile
%----------------------------------------------------------------------
-spec get_last_tile(list()) -> tile_coords().
get_last_tile(TileList) ->
    [Last | _Rest] = lists:reverse(TileList),
    Last#tile.coordinates.

%%=========================================================================
%% Internal functions
%%=========================================================================

%% TODO: Cleanup or used unused/unexported functions
is_tile(empty, Tile) when Tile#tile.occupant =:= [] ->
    ok;
%is_tile(occupied, Tile) when Tile#tile.occupant =/= [] ->
%    ok;
is_tile(not_a_wall, Tile) when Tile#tile.type =/= w ->
    ok;
is_tile(_, _Tile) ->
    false.

%%is_within_range(TileFrom, TileTo) ->
%%	% Perhaps add range modifiers on mobs.
%%	{FromX, FromY} = TileFrom#tile.coordinates,
%%	{ToX, ToY} = TileTo#tile.coordinates,
%%	Distance = math:sqrt((ToX * ToX) + (ToY * ToY)),
%%	Distance =< ?SHIP_MAX_RANGE.

get_tile(Coordinates, TileList) ->
    lists:keyfind(Coordinates, #tile.coordinates, TileList).

update_tile(Coordinates, Tile, TileList) ->
    lists:keyreplace(Coordinates, #tile.coordinates, TileList, Tile).

%---------------------------------------------------------------------------
% @doc The repack function takes a sector list (which is a coordinate pair
%      + index of the tile type corresponding to a wang tile) and
%      enumerates all of the individual tiles. This enumerate list is known
%      to the server but doesn't necessarily need to be known to the player
% @end
%---------------------------------------------------------------------------
-spec repack(list(), tuple()) -> list().
repack(SectorList, GridSize) ->
    ListOfSectorCoords = [populate(K, GridSize, V) || {K, V} <- SectorList],
    RepackedList = lists:merge(ListOfSectorCoords),
    % add a field for flags
    [
        #tile{coordinates = Coordinates, type = Type, flags = []}
     || {Coordinates, Type} <- RepackedList
    ].

%---------------------------------------------------------------------------
% @doc The populate function enumerates all subtiles of the sector by first
%      matching the Index to a 2D array of tuples containing the initial
%      states of each tile. It then takes the enumerated initial states and
%      assigns a coordinate for each tile at the 'room' scope.
% @end
%---------------------------------------------------------------------------
-spec populate(
    {pos_integer(), pos_integer()},
    {pos_integer(), pos_integer()},
    pos_integer()
) -> list().
populate(SectorCoords, GridSize, Index) ->
    % 5x5 grid, row by row starting from 0,0 to 5,5
    % First get all of the subtiles for the index and their initial state
    {Xg, Yg} = GridSize,
    SubTiles =
        case Index of
            15 -> ?SECTOR_15;
            14 -> ?SECTOR_14;
            13 -> ?SECTOR_13;
            12 -> ?SECTOR_12;
            11 -> ?SECTOR_11;
            10 -> ?SECTOR_10;
            9 -> ?SECTOR_9;
            7 -> ?SECTOR_7;
            6 -> ?SECTOR_6;
            5 -> ?SECTOR_5;
            3 -> ?SECTOR_3
        end,
    % For each subtile, we want to transform
    %   Subtile -> {Xs, Ys, Subtile}
    Map = maps:new(),
    % OK, we have to subtract 1 here because we want to 0-index
    % i.e., the coodinates should start at {0,0} and go to {Xg - 1, Yg - 1}

    % magic, based on the size of the SubTiles
    Scale = 5,
    maps:to_list(
        boardy(Xg - 1, Yg - 1, Map, SubTiles, SectorCoords, Scale)
    ).

%-------------------------------------------------------------------------------
% @doc The floodfill function attempts to take all floor tiles and add a flag
%      "F" to them. Once the flag has been added ,it invokes 4 more copies of
%      itself recursively to move one grid spacing above, below, to the left,
%      and to the right. The floodfill does not add flags to wall tiles or any
%      other type of tile.
% @end
%-------------------------------------------------------------------------------
-spec floodfill(list(), pos_integer(), pos_integer()) -> list().
floodfill(TileList, X, Y) ->
    % this seems expensive in terms of algorithmic complexity?
    % replace this with a map later, perhaps
    Tile = lists:keyfind({X, Y}, #tile.coordinates, TileList),
    case Tile of
        % if there is already a flag here, do nothing
        Tile when length(Tile#tile.flags) > 0 ->
            TileList;
        % if the tile is a wall, do nothing
        Tile when Tile#tile.type == ?WALL ->
            TileList;
        % if the key doesn't exist (i.e., out of bounds), also do nothing
        false ->
            TileList;
        _ ->
            NewTile = Tile#tile{flags = [?FLOOD]},
            NewTileList = lists:keyreplace(
                {X, Y},
                #tile.coordinates,
                TileList,
                NewTile
            ),
            % each subsequent func needs to use the updated board from the last one
            B1 = floodfill(NewTileList, X + 1, Y),
            B2 = floodfill(B1, X, Y + 1),
            B3 = floodfill(B2, X - 1, Y),
            floodfill(B3, X, Y - 1)
    end.

%-------------------------------------------------------------------------------
% @doc Recurses through the list looking for an instance of a floor that does
%      not have a Flooded flag (F). If the flag doesn't exist, this tile is
%      unreachable and the function returns false.
%      Otherwise, if all tiles are reachable, the function returns true.
% @end
%-------------------------------------------------------------------------------
-spec check_flooded_tiles(list()) -> boolean().
check_flooded_tiles([]) ->
    true;
check_flooded_tiles([Head | Tail]) when Head#tile.type == ?FLOOR ->
    case Head#tile.flags of
        % If the tile is not flooded then the map is invalid
        [] -> false;
        _ -> check_flooded_tiles(Tail)
    end;
% if the tile is any type other than floor, it's probably ok to skip
check_flooded_tiles([_Head | Tail]) ->
    check_flooded_tiles(Tail).

%-------------------------------------------------------------------------------
% @doc The boardx and boardy functions are functions that build up a map that
%      reflects the 2D grid laid out by the "magic" values for each sector.
%      This function is unfortunately quite heady and could use some serious
%      tough love.
% @end
%-------------------------------------------------------------------------------
boardy(X, Y, Map, Subtile, Sector, Scale) when Y == 0 ->
    boardx(X, Y, Map, Subtile, Sector, Scale);
boardy(X, Y, Map, Subtile, Sector, Scale) ->
    Map2 = boardx(X, Y, Map, Subtile, Sector, Scale),
    boardy(X, Y - 1, Map2, Subtile, Sector, Scale).

boardx(X, Y, Map, Subtile, Sector, Scale) when X == 0 ->
    % Elements in a tuple are 1-indexed, so we have to add the 1 back
    {Xs, Ys} = Sector,
    Element = element(X + 1, element(Y + 1, Subtile)),
    % once the element is plucked out, we append it to the map with the sector
    % coordinates
    % TODO The magic "5" values correspond to te "Tiles per Grid" value that we
    % need to slap into the turfwar magic file
    % NOTE: I am not sure if it works with non-rectangular grids.
    maps:put({X + (Xs * Scale), Y + (Ys * Scale)}, Element, Map);
boardx(X, Y, Map, Subtile, Sector, Scale) ->
    % Elements in a tuple are 1-indexed, so we have to add the 1 back
    {Xs, Ys} = Sector,
    Element = element(X + 1, element(Y + 1, Subtile)),
    % once the element is plucked out, we append it to the map with the sector coordinates
    Map2 = maps:put({X + (Xs * Scale), Y + (Ys * Scale)}, Element, Map),
    boardx(X - 1, Y, Map2, Subtile, Sector, Scale).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wang Tiler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec newMap(pos_integer(), pos_integer()) -> list().
newMap(X, Y) ->
    M0 = tile(X, Y),
    % are Map Comprehensions broken/missing in OTP 22 ?
    [{K, binpack(V)} || {K, V} <- maps:to_list(M0)].

binpack(Bin) ->
    % the original form as produced by the wanger tiler i.e., <<1,0,1,1>>
    % correspondin to <<W,S,E,N>>
    % These are atually 8 bits each ,s owe want to repack them into a single 4 bit
    % binary so we can get the index number back out
    <<Index:4>> = <<<<X:1>> || <<X:8>> <= Bin>>,
    Index.

tile(X, Y) ->
    % at X max and Y max, we create a map with a random starting corner
    %F = fun(N) -> tile(X - 1, N, Map) end,
    %Map = #{ {X,Y} => wangHelper(randomTile(), randomTile()) },
    Map = maps:new(),
    ytile(X, Y, Map).

ytile(X, Y, Map) when Y == 0 ->
    xtile(X, Y, Map);
ytile(X, Y, Map) ->
    Map2 = xtile(X, Y, Map),
    ytile(X, Y - 1, Map2).

xtile(X, Y, Map) when X == 0 ->
    % we're done, stop the recursion
    RightTile = maps:get(
        {X + 1, Y},
        Map,
        reducedWang(randomTile(), randomTile())
    ),
    BelowTile = maps:get(
        {X, Y + 1},
        Map,
        reducedWang(randomTile(), randomTile())
    ),
    maps:put({X, Y}, reducedWang(RightTile, BelowTile), Map);
xtile(X, Y, Map) ->
    RightTile = maps:get(
        {X + 1, Y},
        Map,
        reducedWang(randomTile(), randomTile())
    ),
    BelowTile = maps:get(
        {X, Y + 1},
        Map,
        reducedWang(randomTile(), randomTile())
    ),
    Map2 = maps:put({X, Y}, reducedWang(RightTile, BelowTile), Map),
    xtile(X - 1, Y, Map2).

randomTile() ->
    TileList = [rand:uniform(2) - 1 || _ <- lists:seq(1, 4)],
    binary:list_to_bin(TileList).

wangHelper(RightTile, BelowTile) ->
    % the canonical direction is:
    %   << W, S, E, N >>
    % Ensure that we always match below and to the right
    % South and East of me is North and West of my pal
    <<E, _, _, _>> = RightTile,
    <<_, _, _, S>> = BelowTile,
    W = rand:uniform(2) - 1,
    N = rand:uniform(2) - 1,
    <<W, S, E, N>>.

reducedWang(RightTile, BelowTile) ->
    % Reduced Wang has a series of acceptors
    % If the proposed tile from the wangHelper is on a deny list, then it asks
    % the wangHelper to create a new one until a valid tile is created
    % Reminder that the canonical direction is:
    %   << W, S, E, N >>
    % Accept seems better than reject for higher order tilings. Maybe move the
    % tile indices to a macro
    % i.e., 2edge2path wang has 2^8 possible tiles but there are about 47 valid ones
    % This removes dead ends and completely empty + completely full blocks
    Tile = wangHelper(RightTile, BelowTile),
    case Tile of
        % index 3
        <<0, 0, 1, 1>> -> <<0, 0, 1, 1>>;
        % index 5
        <<0, 1, 0, 1>> -> <<0, 1, 0, 1>>;
        % index 6
        <<0, 1, 1, 0>> -> <<0, 1, 1, 0>>;
        % index 7
        <<0, 1, 1, 1>> -> <<0, 1, 1, 1>>;
        % index 9
        <<1, 0, 0, 1>> -> <<1, 0, 0, 1>>;
        % index 10
        <<1, 0, 1, 0>> -> <<1, 0, 1, 0>>;
        % index 11
        <<1, 0, 1, 1>> -> <<1, 0, 1, 1>>;
        % index 12
        <<1, 1, 0, 0>> -> <<1, 1, 0, 0>>;
        % index 13
        <<1, 1, 0, 1>> -> <<1, 1, 0, 1>>;
        % index 14
        <<1, 1, 1, 0>> -> <<1, 1, 1, 0>>;
        % index 15
        <<1, 1, 1, 1>> -> <<1, 1, 1, 1>>;
        % everything else
        _ -> reducedWang(RightTile, BelowTile)
    end.


% Unit tests


