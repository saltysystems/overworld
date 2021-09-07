-module(goblet_shipgrid).

-include("game/goblet_shipgrid.hrl").

-export([new/0, put/3, get/2, floodfill/2]).

-type coords() :: {pos_integer(), pos_integer()}.
-type ttype() :: empty | component.

-record(tile, {
          type = empty :: ttype(),
          flags=[]
         }).

-spec new() -> map().
new() ->
    goblet_grid:new(?SHIPGRID_SIZE_X,?SHIPGRID_SIZE_Y, #tile{}).

-spec put(coords(), ttype(), map()) -> map().
put(Coords, Type, Map) ->
    goblet_grid:put(Coords, #tile{type=Type}, Map).

-spec get(coords(), map()) -> map().
get(Coords, Map) ->
    goblet_grid:get(Coords, Map).

%TODO: Generalize this .. maybe a general "tile" record that holds a type,
%      flags..
-spec floodfill(coords(), map()) -> map().
floodfill({X,Y}, Map0) ->
    Tile = maps:get({X,Y}, Map0, out_of_bounds),
    case Tile of
        % if there is already a flag here, do nothing
        Tile when length(Tile#tile.flags) > 0 ->
            Map0;
        % if the tile is empty, do nothing
        Tile when Tile#tile.type == empty ->
            Map0;
        % if the key doesn't exist (i.e., out of bounds), also do nothing
        out_of_bounds ->
            Map0;
        _ ->
			% Put the atom 'f' into the flags to set the flag
            NewTile = Tile#tile{flags = [f]}, 
            Map1 = maps:put({X,Y}, NewTile, Map0),
            % each subsequent func needs to use the updated board from the last one
            Map2 = floodfill({X+1,Y}, Map1),
            Map3 = floodfill({X, Y + 1}, Map2),
            Map4 = floodfill({X - 1, Y}, Map3),
            floodfill({X,Y-1}, Map4)
	end.
