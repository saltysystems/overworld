-module(goblet_shipgrid).

-include("game/goblet_shipgrid.hrl").

-export([new/0, put_cell/3, get_cell/2, valid_adjacent/3, apply_rotation/2]).

-type coords() :: {pos_integer(), pos_integer()}.
-type ttype() :: empty | component.
%-type wang_bitmask() :: [pos_integer(), pos_integer(), pos_integer(), pos_integer()].
-type wang_bitmask() :: <<_:4>>.
-type wang_index() :: 0..15.
-type direction() :: north | south | east | west.
-type rotation() ::  0 | 1 | 2 | 3.

-record(cell, {
          type = empty :: ttype(),
          id = "",
          flags=[],
          wang_bitmask= <<0,0,0,0>> :: wang_bitmask() % <<W,S,E,N>>
         }).

-spec new() -> map().
new() ->
    goblet_grid:new(?SHIPGRID_SIZE_X,?SHIPGRID_SIZE_Y, #cell{}).

-spec put_cell(coords(), ttype(), map()) -> map().
put_cell(Coords, ID, Map) ->
    put_cell(Coords, ID, 0, Map).

-spec put_cell(coords(), ttype(), rotation(), map()) -> map().
put_cell(Coords, ID, Rotation, Map) ->
    % Lookup the wang index, flags, etc.
    WangIndex = apply_rotation(goblet_db:item_wang_bitmask(ID), Rotation),
    goblet_grid:put(Coords, #cell{id=ID, type=component, wang_bitmask=WangIndex}, Map).

-spec get_cell(coords(), map()) -> map().
get_cell(Coords, Map) ->
    goblet_grid:get(Coords, Map).

% Rotate the specified cell by bitshifting Index by R.
% e.g., <<0,0,1,1>> -> <<0,1,1,0>>
-spec apply_rotation(wang_index(), rotation()) -> binary().
apply_rotation(Index, R) when is_integer(Index) ->
    apply_rotation(index_to_bitstring(Index), R);
apply_rotation(<<A,B,C,D>>, 0) ->
    <<A,B,C,D>>;
apply_rotation(<<A,B,C,D>>, R) ->
    apply_rotation(<<B,C,D,A>>, R-1).

index_to_bitstring(Index) ->
    << <<X:8>> || <<X:1>> <= <<Index:4>> >>.

-spec valid_adjacent(coords(),coords(),map()) -> boolean().
% Given two coordinate pairs, determine if the wang tiling is valid.
valid_adjacent({X1,Y1} = Coords1, {X2,Y2} = Coords2, Map) ->
    Cell1 = get_cell(Coords1, Map),
    Cell2 = get_cell(Coords2, Map),
    % Subtract the coordinates to get 
    Delta = {X2-X1,Y2-Y1},
    % Tile origin is canonically top left. 
    %   +X -> East
    %   -X -> West
    %   +Y -> North
    %   -Y -> South
    case Delta of 
        {1,0}  -> wang_adjacent(Cell1#cell.wang_bitmask, Cell2#cell.wang_bitmask, east);
        {-1,0} -> wang_adjacent(Cell1#cell.wang_bitmask, Cell2#cell.wang_bitmask, west);
        {0,1}  -> wang_adjacent(Cell1#cell.wang_bitmask, Cell2#cell.wang_bitmask, north);
        {0,-1} -> wang_adjacent(Cell1#cell.wang_bitmask, Cell2#cell.wang_bitmask, south)
    end.

-spec wang_adjacent(wang_bitmask(), wang_bitmask(), direction()) -> boolean().
% For a given cell C2, which lay Direction relative to C1, determine if the
% orientation is correct per the Wang index.
wang_adjacent(<<_,_,_,1>>, <<_,1,_,_>>, north) ->
    true;
wang_adjacent(<<_,1,_,_>>, <<_,_,_,1>>, south) ->
    true;
wang_adjacent(<<_,_,1,_>>, <<1,_,_,_>>, east) ->
    true;
wang_adjacent(<<1,_,_,_>>, <<_,_,1,_>>, west) ->
    true;
wang_adjacent(_C1, _C2, _Direction) ->
    false.
