-module(goblet_shipgrid).

-include("game/goblet_shipgrid.hrl").

-export([
    new/0,
    put_cell/3, put_cell/4,
    get_cell/2,
    validate/1,
    apply_rotation/2
]).

-type coords() :: {pos_integer(), pos_integer()}.
-type wang_bitmask() :: <<_:4>> | <<_:32>>.
-type wang_index() :: 0..15.
-type direction() :: north | south | east | west.
-type rotation() :: 0 | 1 | 2 | 3.

-record(cell, {
    type :: atom(),
    object :: goblet_ship:component() | undefined,
    % <<W,S,E,N>>
    wang_bitmask = <<0, 0, 0, 0>> :: wang_bitmask()
}).

-opaque cell() :: #cell{}.

-export_type([cell/0]).

-spec new() -> map().
new() ->
    goblet_grid:new(?SHIPGRID_SIZE_X, ?SHIPGRID_SIZE_Y, #cell{}).

-spec put_cell(coords(), goblet_ship:component(), map()) -> map().
put_cell(Coords, Component, Map) ->
    put_cell(Coords, Component, 0, Map).

-spec put_cell(coords(), goblet_ship:component(), rotation(), map()) ->
    map().
put_cell(Coords, Component, Rotation, Map) ->
    % Lookup the wang index, flags, etc.
    Index = goblet_ship:wang_index(Component),
    goblet_grid:put_obj(
        Coords,
        #cell{
            object = Component,
            type = component,
            wang_bitmask = apply_rotation(Index, Rotation)
        },
        Map
    ).

-spec get_cell(coords(), map()) -> cell().
get_cell(Coords, Map) ->
    goblet_grid:get_obj(Coords, Map).

% Rotate the specified cell by bitshifting Index by R.
% e.g., <<0,0,1,1>> -> <<0,1,1,0>>
-spec apply_rotation(wang_index() | bitstring(), rotation()) -> binary().
apply_rotation(Index, R) when is_integer(Index) ->
    apply_rotation(index_to_bitstring(Index), R);
apply_rotation(<<A, B, C, D>>, 0) ->
    <<A, B, C, D>>;
apply_rotation(<<A, B, C, D>>, R) ->
    apply_rotation(<<B, C, D, A>>, R - 1).

index_to_bitstring(Index) ->
    <<<<X:8>> || <<X:1>> <= <<Index:4>>>>.

-spec validate(map()) -> boolean().
% Validate a ship layout in a couple of passes.
validate(Map) ->
    % Get the first non-empty tile.
    Cells = goblet_grid:get_nonempty(Map),
    CoordList = goblet_grid:get_coordinate_list(Cells),
    [Coords | _] = CoordList,
    case goblet_grid:is_contiguous(Coords, Map) of
        false ->
            false;
        _ ->
            valid_wang_tree(CoordList, Map, [])
    end.

-spec valid_wang_tree([coords(), ...], map(), [] | [boolean(), ...]) ->
    boolean().
valid_wang_tree([], _Map, Acc) ->
    % Logically AND all of the boolean values.
    lists:foldl(fun(X, Bool) -> X and Bool end, true, Acc);
valid_wang_tree(Components = [{X, Y} | Remaining], Map, Acc) ->
    CheckList = [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}],
    Intersection = [A || A <- Components, B <- CheckList, A =:= B],
    Valids = [valid_adjacent(Next, {X, Y}, Map) || Next <- Intersection],
    valid_wang_tree(Remaining, Map, Valids ++ Acc).

-spec valid_adjacent(coords(), coords(), map()) -> boolean().
% Given two coordinate pairs, determine if the wang tiling is valid.
valid_adjacent({X1, Y1} = Coords1, {X2, Y2} = Coords2, Map) ->
    Cell1 = get_cell(Coords1, Map),
    Cell2 = get_cell(Coords2, Map),
    % Subtract the coordinates to get
    Delta = {X2 - X1, Y2 - Y1},
    % Tile origin is canonically top left.
    %   +X -> East
    %   -X -> West
    %   +Y -> North
    %   -Y -> South
    case Delta of
        {1, 0} ->
            wang_adjacent(
                Cell1#cell.wang_bitmask,
                Cell2#cell.wang_bitmask,
                east
            );
        {-1, 0} ->
            wang_adjacent(
                Cell1#cell.wang_bitmask,
                Cell2#cell.wang_bitmask,
                west
            );
        {0, 1} ->
            wang_adjacent(
                Cell2#cell.wang_bitmask,
                Cell1#cell.wang_bitmask,
                north
            );
        {0, -1} ->
            wang_adjacent(
                Cell2#cell.wang_bitmask,
                Cell1#cell.wang_bitmask,
                south
            )
    end.

-spec wang_adjacent(wang_bitmask(), wang_bitmask(), direction()) ->
    boolean().
% For a given cell C2, which lay Direction relative to C1, determine if the
% orientation is correct per the Wang index.
wang_adjacent(<<_, _, _, 1>>, <<_, 1, _, _>>, north) ->
    true;
wang_adjacent(<<_, 1, _, _>>, <<_, _, _, 1>>, south) ->
    true;
wang_adjacent(<<_, _, 1, _>>, <<1, _, _, _>>, east) ->
    true;
wang_adjacent(<<1, _, _, _>>, <<_, _, 1, _>>, west) ->
    true;
wang_adjacent(_C2, _C1, _Direction) ->
    false.
