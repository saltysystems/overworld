-module(ow_grid2d).

-export([
    new/2, new/3,
    get_col/2,
    put_col/3,
    get_row/2,
    put_row/3,
    put/3,
    get/2,
    floodfill/2,
    is_contiguous/2,
    get_empty/1,
    get_nonempty/1,
    get_flooded/1,
    get_unflooded/1,
    set_flag/3,
    unset_flag/3,
    has_flag/3,
    distance/2,
    get_coordinate_list/1
]).

-type coords() :: {pos_integer(), pos_integer()}.

-record(tile, {flags = #{}, object}).

%TODO: Can we just make this without the initialization step? How does this
%      affect floodfill etc?
-spec new(pos_integer(), pos_integer()) -> map().
new(M, N) ->
    new(M, N, undefined).

-spec new(pos_integer(), pos_integer(), any()) -> map().
new(N, M, Object) ->
    Coords = [{R, C} || R <- lists:seq(1, M), C <- lists:seq(1, N)],
    lists:foldl(
        fun(X, Map) -> maps:put(X, #tile{object = Object}, Map) end,
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
    M1 = maps:put({X, Y}, Tile, Map),
    set_flag({X, Y}, nonempty, M1).

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
floodfill(Coordinates, Map0) ->
    case maps:get(Coordinates, Map0, out_of_bounds) of
        out_of_bounds ->
            Map0;
        _ ->
            NonEmpty = has_flag(Coordinates, nonempty, Map0),
            Flooded = has_flag(Coordinates, flood, Map0),
            floodfill(Coordinates, false, NonEmpty, Flooded, Map0)
    end.

floodfill({X, Y}, false, true, false, Map0) ->
    Map1 = set_flag({X, Y}, flood, Map0),
    % each subsequent func needs to use the updated board
    % from the last one.. lists:foldl ?
    Map2 = floodfill({X + 1, Y}, Map1),
    Map3 = floodfill({X, Y + 1}, Map2),
    Map4 = floodfill({X - 1, Y}, Map3),
    floodfill({X, Y - 1}, Map4);
floodfill(_Coords, _OOB, _NonEmpty, _Flooded, Map0) ->
    Map0.

% Performs a floodfill starting at the specified coordinates.
% If all defined tiles (i.e., #tile.object != 'undefined') are flooded, then
% return 'true'. otherwise return 'false'
-spec is_contiguous(coords(), map()) -> boolean().
is_contiguous(Coords, Map0) ->
    Map1 = floodfill(Coords, Map0),
    % First get all non-empty tiles:
    %
    % TODO: Broken - there's no way for ow_grid to know about "nonempty"
    % when we put semi-initialized cells into the object field e.g. thru
    % ow_ship_grid:new().
    %
    Map2 = get_nonempty(Map1),
    % Get all tiles with a flag
    Map3 = get_flooded(Map2),
    % If the map is unchanged, then all objects are connected and the tile is
    % floodable.  Otherwise the map is not floodable.
    Map3 == Map2.

-spec get_nonempty(map()) -> map().
get_nonempty(Map) ->
    % feels hackish. TODO - decide if its worth cleaning this up. as is, we
    % pass the entire map into a map predicate, which feels a bit funny. same
    % with get_empty/1
    Pred = fun({R, C}, _V) -> has_flag({R, C}, nonempty, Map) == true end,
    maps:filter(Pred, Map).

-spec get_empty(map()) -> map().
get_empty(Map) ->
    Pred = fun({R, C}, _V) -> has_flag({R, C}, nonempty, Map) == false end,
    maps:filter(Pred, Map).

-spec get_flooded(map()) -> map().
get_flooded(Map) ->
    Pred = fun({_R, _C}, V) ->
        maps:get(flood, V#tile.flags, false) == true
    end,
    maps:filter(Pred, Map).

-spec get_unflooded(map()) -> map().
get_unflooded(Map) ->
    Pred = fun({_R, _C}, V) ->
        maps:get(flood, V#tile.flags, false) == false
    end,
    maps:filter(Pred, Map).

-spec set_flag(coords(), any(), map()) -> map().
set_flag(Coordinates, Flag, Map0) ->
    Tile0 = maps:get(Coordinates, Map0),
    Tile1 = Tile0#tile{flags = maps:put(Flag, true, Tile0#tile.flags)},
    maps:put(Coordinates, Tile1, Map0).

-spec unset_flag(coords(), any(), map()) -> map().
unset_flag(Coordinates, Flag, Map0) ->
    Tile0 = maps:get(Coordinates, Map0),
    Tile1 = Tile0#tile{flags = maps:remove(Flag, Tile0#tile.flags)},
    maps:put(Coordinates, Tile1, Map0).

-spec has_flag(coords(), any(), map()) -> boolean().
has_flag(Coordinates, Flag, Map0) ->
    Tile = maps:get(Coordinates, Map0),
    maps:get(Flag, Tile#tile.flags, false).

-spec distance(coords(), coords()) -> pos_integer().
distance({X1, Y1}, {X2, Y2}) ->
    abs(X2 - X1) + abs(Y2 - Y1).

-spec get_coordinate_list(map()) -> list().
get_coordinate_list(Map) ->
    L = maps:to_list(Map),
    [Coords || {Coords, _Val} <- L].
