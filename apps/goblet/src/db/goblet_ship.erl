-module(goblet_ship).

-include_lib("eunit/include/eunit.hrl").

-include("db/goblet_database.hrl").

% Functions exported to the scripting interface
-export([
    new/5,
    delete/1,
    get/1,
%    put/1,
    id/1,
    name/1,
    wang_index/1,
    appearance/1,
    attributes/1,
    to_map/1
]).

-type id() :: non_neg_integer().
-type wang_index() :: 0..15.
-type type() :: string().
-type name() :: string().
-type appearance() :: non_neg_integer().
-type attributes() :: map().
-opaque component() :: #goblet_ship_component{}.
-export_type([component/0]).

-spec new(name(), wang_index(), appearance(), type(), map()) -> id() | {error, atom()}.
new(Name, WangIndex, Type, Appearance, Attributes) when is_map(Attributes) ->
    Fun = fun() ->
        NextID = mnesia:dirty_update_counter(
            goblet_table_ids,
            goblet_ship_component,
            1
        ),
        mnesia:write(
            goblet_ship_component,
            #goblet_ship_component{
                name = Name,
                id = NextID,
                wang_index = WangIndex,
                type = Type,
                appearance = Appearance,
                attributes = Attributes
            },
            write
        ),
        NextID
    end,
    mnesia:activity(transaction, Fun).

-spec delete(id() | component()) -> ok | {error, atom()}.
delete(Component) when is_record(Component, goblet_ship_component) ->
    delete(id(Component));
delete(ID) when is_integer(ID) ->
    Fun = fun() ->
        % Items can't be removed simply without orphan objects in
        % player inventories, so this is quite complex.
        % First, check all players for a copy of the item.
        % Remove the item from the player's inventory.
        % Then finally delete the item itself.
        mnesia:foldl(
            fun(Rec, Acc) ->
                Inventory = Rec#goblet_player.inventory,
                case lists:member(ID, Inventory) of
                    true ->
                        I2 = lists:delete(ID, Inventory),
                        mnesia:write(
                            goblet_player,
                            Rec#goblet_player{inventory = I2},
                            write
                        ),
                        [ok | Acc];
                    _ ->
                        Acc
                end
            end,
            [],
            goblet_player
        ),
        mnesia:delete({goblet_ship_component, ID})
    end,
    mnesia:activity(transaction, Fun).

-spec get(id()) -> component().
get(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component;
            [] ->
                {error, no_such_ship_component}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec id(component()) -> id().
id(Component) -> 
    Component#goblet_ship_component.id.

-spec name(component()) -> name().
name(Component) -> 
    Component#goblet_ship_component.name.

-spec wang_index(component()) -> wang_index().
wang_index(Component) -> 
    Component#goblet_ship_component.wang_index.

-spec appearance(component()) -> appearance().
appearance(Component) -> 
    Component#goblet_ship_component.appearance.

-spec attributes(component()) -> attributes().
attributes(Component) -> 
    Component#goblet_ship_component.attributes.

% Convert the opaque object into a simple map - useful for e.g. GDMinus.
% Note that the map is sort of "read only" - no destructive updates to the map.
-spec to_map(component()) -> #{name := name(), id := id(), wang_index := wang_index(), type := type(), appearance := appearance(), attributes := attributes()}.
to_map(Component) ->
    #{
      name=>Component#goblet_ship_component.name,
      id=>Component#goblet_ship_component.id,
      wang_index=>Component#goblet_ship_component.wang_index,
      type=>Component#goblet_ship_component.type,
      appearance=>Component#goblet_ship_component.appearance,
      attributes=>Component#goblet_ship_component.attributes
     }.
