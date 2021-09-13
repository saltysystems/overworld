-module(goblet_ship_component).

-include_lib("eunit/include/eunit.hrl").

-include("db/goblet_database.hrl").

% Functions exported to the scripting interface
-export([
         new/3,
         new/4,
         delete/1,
         wang_index/1,
         name/1,
         attributes/1
   ]).

-type id() :: pos_integer().
-type wang_index() :: 0..15.
-type component_type() :: engine | weapon | cargo | defensive. % WIP

-spec new(list(), wang_index(), component_type()) -> id().
new(Name, WangIndex, Type) ->
   new(Name, WangIndex, Type, maps:new()).
-spec new(list(), wang_index(), component_type(), map()) -> id().
new(Name, WangIndex, Type, Attributes) when is_map(Attributes) ->
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
                        attributes = Attributes
                    },
                    write),
                NextID
    end,
    mnesia:activity(transaction, Fun).


-spec delete(id()) -> ok | {error, atom()}.
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

-spec wang_index(id()) -> wang_index() | {error, atom()}.
wang_index(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component#goblet_ship_component.wang_index;
            [] -> 
                {error, no_such_ship_component}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec name(id()) -> string() | {error, atom()}.
name(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component#goblet_ship_component.name;
            [] -> 
                {error, no_such_ship_component}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec attributes(id()) -> map() | {error, atom()}.
attributes(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component#goblet_ship_component.attributes;
            [] -> 
                {error, no_such_ship_component}
        end
    end,
    mnesia:activity(transaction, Fun).
