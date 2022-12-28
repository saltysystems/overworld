-module(ow_sparsegrid).

-export([
    new/0,
    col/2,
    col/3,
    row/3,
    row/2,
    put/4,
    put/3,
    get/2,
    floodfill/2,
    is_contiguous/2,
    set_flag/3,
    has_flag/3,
    get_flag/2,
    whereis/2
]).

% @doc
%   This module defines a new version of grid2d with a sparse grid.
% @end
-type coords() :: ow_vector:vector().

new() ->
    #{}.

-spec col(pos_integer(), map(), interpolate | {interpolate, any()}) ->
    map().
col(Col, SparseMap, interpolate) ->
    col(Col, SparseMap, {interpolate, undefined});
col(Col, SparseMap, {interpolate, DefaultVal}) ->
    SparseCol = col(Col, SparseMap),
    % Erlang map terms are not guaranteed to be ordered, so must convert to
    % list and order them and build a new map
    Keys = lists:sort(maps:to_list(SparseCol)),
    interpolate_col(Keys, DefaultVal, 0, #{}).

-spec col(pos_integer(), map()) -> map().
col(Col, Map) ->
    Pred = fun({C, _R}, _V) -> C == Col end,
    maps:filter(Pred, Map).

-spec row(pos_integer(), map(), interpolate | {interpolate, any()}) ->
    map().
row(Row, Map, interpolate) ->
    row(Row, Map, {interpolate, undefined});
row(Row, SparseMap, {interpolate, DefaultVal}) ->
    SparseRow = row(Row, SparseMap),
    % Erlang map terms are not guaranteed to be ordered, so must convert to
    % list and order them and build a new map
    Keys = lists:sort(maps:to_list(SparseRow)),
    interpolate_row(Keys, DefaultVal, 0, #{}).

-spec row(pos_integer(), map()) -> map().
row(Row, Map) ->
    Pred = fun({_C, R}, _V) -> R == Row end,
    maps:filter(Pred, Map).

-spec get(coords(), map()) -> any().
get({X, Y}, Map) ->
    #{data := Data} = maps:get({X, Y}, Map),
    Data.

-spec put(coords(), any(), map()) -> map().
put({X, Y}, What, Map) ->
    put({X, Y}, What, nonempty, Map).

-spec put(coords(), any(), any(), map()) -> map().
put({X, Y}, What, Flag, Map) ->
    Tile = #{data => What, flag => Flag},
    Map#{{X, Y} => Tile}.

-spec set_flag(coords(), any(), map()) -> map().
set_flag({X, Y}, Flag, Map) ->
    Tile0 = maps:get({X, Y}, Map),
    Tile1 = Tile0#{flag => Flag},
    Map#{{X, Y} => Tile1}.

-spec has_flag(coords(), any(), map()) -> boolean().
has_flag({X, Y}, Flag, Map0) ->
    Tile = maps:get({X, Y}, Map0),
    maps:get(flag, Tile) == Flag.

-spec get_flag(any(), map()) -> map().
get_flag(Flag, Map) ->
    Pred = fun({X, Y}, _V) -> has_flag({X, Y}, Flag, Map) == true end,
    maps:filter(Pred, Map).

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
    Map2 = get_flag(nonempty, Map1),
    % Get all tiles with a flag
    Map3 = get_flag(flooded, Map2),
    % If the map is unchanged, then all objects are connected and the tile is
    % floodable.  Otherwise the map is not floodable.
    Map3 == Map2.

-spec whereis(any(), map()) -> [coords()].
whereis(Val, Map) ->
    Pred = fun({X, Y}, #{data := Data}, AccIn) ->
        if
            Val == Data ->
                [{X, Y} | AccIn];
            true ->
                AccIn
        end
    end,
    maps:fold(Pred, [], Map).

%%-------------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------------

interpolate_row([], _Val, _Last, Map) ->
    Map;
interpolate_row([{{C, R}, _Data} | _Tail] = KV, Val, Last, Map) when
    C - Last > 1
->
    % Need to interpolate this column
    Map1 = put({Last + 1, R}, Val, empty, Map),
    interpolate_row(KV, Val, Last + 1, Map1);
interpolate_row([{{C, R}, Data} | Tail], Val, _Last, Map) ->
    interpolate_row(Tail, Val, C, Map#{{C, R} => Data}).

interpolate_col([], _Val, _Last, Map) ->
    Map;
interpolate_col([{{C, R}, _Data} | _Tail] = KV, Val, Last, Map) when
    R - Last > 1
->
    % Need to interpolate this column
    Map1 = put({C, Last + 1}, Val, empty, Map),
    interpolate_col(KV, Val, Last + 1, Map1);
interpolate_col([{{C, R}, Data} | Tail], Val, _Last, Map) ->
    interpolate_col(Tail, Val, R, Map#{{C, R} => Data}).
