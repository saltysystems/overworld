-module(goblet_grid).

-export([
    new/1, new/2,
    get_col/2,
    put_col/3,
    get_row/2,
    put_row/3,
    put/3,
    get/2,
    floodfill/2,
    is_contiguous/2
]).

-type coords() :: {pos_integer(), pos_integer()}.

-record(tile, {flags = [], object}).

% Create a square grid of size Size.
-spec new(pos_integer()) -> map().
new(Size) ->
    new(Size, Size).

-spec new(pos_integer(), pos_integer()) -> map().
new(N, M) ->
    Coords = [{R, C} || R <- lists:seq(1, M), C <- lists:seq(1, N)],
    lists:foldl(
        fun(X, Map) -> maps:put(X, #tile{}, Map) end,
        maps:new(),
        Coords
    ).

-spec get(coords(), map()) -> any().
get({X, Y}, Map) ->
    Tile = maps:get({X, Y}, Map),
    Tile#tile.object.

-spec get_col(pos_integer(), map()) -> map().
get_col(Col, Map) ->
    Pred = fun({_R, C}, _V) -> C == Col end,
    maps:filter(Pred, Map).

-spec get_row(pos_integer(), map()) -> map().
get_row(Row, Map) ->
    Pred = fun({R, _C}, _V) -> R == Row end,
    maps:filter(Pred, Map).

%-spec get_subgrid(StartCoords, EndCoords, map()) -> map().

-spec put(coords(), any(), map()) -> map().
put({X, Y}, What, Map) ->
    Tile = #tile{object = What},
    maps:put({X, Y}, Tile, Map).

-spec put_col(pos_integer(), any(), map()) -> map().
put_col(Col, NewVal, Map) ->
    Fun = fun
        ({_R, C}, V) when C == Col -> {true, V#tile{object = NewVal}};
        (_, _) -> false
    end,
    Filter = maps:filtermap(Fun, Map),
    maps:merge(Map, Filter).

-spec put_row(pos_integer(), any(), map()) -> map().
put_row(Row, NewVal, Map) ->
    Fun = fun
        ({R, _C}, V) when R == Row -> {true, V#tile{object = NewVal}};
        (_, _) -> false
    end,
    Filter = maps:filtermap(Fun, Map),
    maps:merge(Map, Filter).

-spec floodfill(coords(), map()) -> map().
floodfill({X, Y}, Map0) ->
    Tile = maps:get({X, Y}, Map0, out_of_bounds),
    case Tile of
        % if there is already a flag here, do nothing
        Tile when length(Tile#tile.flags) > 0 ->
            Map0;
        % if the cell has nothing in it, do nothing
        Tile when Tile#tile.object == undefined ->
            Map0;
        % if the key doesn't exist (i.e., out of bounds), also do nothing
        out_of_bounds ->
            Map0;
        _ ->
            % Put the atom 'f' into the flags to set the flag
            NewTile = Tile#tile{flags = [f]},
            Map1 = maps:put({X, Y}, NewTile, Map0),
            % each subsequent func needs to use the updated board from the last one
            Map2 = floodfill({X + 1, Y}, Map1),
            Map3 = floodfill({X, Y + 1}, Map2),
            Map4 = floodfill({X - 1, Y}, Map3),
            floodfill({X, Y - 1}, Map4)
    end.

% Performs a floodfill starting at the specified coordinates.
% If all defined tiles (i.e., #tile.object != 'undefined') are flooded, then
% return 'true'. otherwise return 'false'
-spec is_contiguous(coords(), map()) -> boolean().
is_contiguous(Coords, Map0) ->
    Map1 = floodfill(Coords, Map0),
    % First get all non-empty tiles:
    Pred1 = fun({_R, _C}, V) -> V =/= #tile{} end,
    Map2 = maps:filter(Pred1, Map1),
    % Now Filter out only the ones that have no flag
    Pred2 = fun({_R, _C}, V) -> V#tile.flags =/= [] end,
    % If the map is unchanged, then all objects are connected and the tile is
    % floodable.  Otherwise the map is not floodable.
    Map3 = maps:filter(Pred2, Map2),
    Map3 == Map2.
