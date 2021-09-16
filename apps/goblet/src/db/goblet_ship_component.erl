-module(goblet_ship_component).

-include_lib("eunit/include/eunit.hrl").

-include("db/goblet_database.hrl").

% Functions exported to the scripting interface
-export([
    new/3,
    new/5,
    delete/1,
    info/1,
    wang_index/1,
    name/1,
    appearance/1,
    attributes/1
]).

-type id() :: non_neg_integer().
-type wang_index() :: 0..15.
-type component_type() :: string().
-type name() :: string().
-type appearance() :: non_neg_integer().

-spec new(name(), wang_index(), component_type()) -> id().
new(Name, WangIndex, Type) ->
    new(Name, WangIndex, Type, 0, maps:new()).

-spec new(name(), wang_index(), appearance(), component_type(), map()) -> id().
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

% This returns an API-safe Map that can be stored as a dict in GDMinus
info(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component;
            [] ->
                {error, no_such_ship_component}
        end
    end,
    Result = mnesia:activity(transaction, Fun),
    #{
      name=>Result#goblet_ship_component.name,
      wang_index=>Result#goblet_ship_component.wang_index,
      type=>Result#goblet_ship_component.type,
      appearance=>Result#goblet_ship_component.appearance,
      attributes=>Result#goblet_ship_component.attributes
     }.




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

-spec appearance(id()) -> non_neg_integer().
appearance(ID) ->
    Fun = fun() ->
        case mnesia:read({goblet_ship_component, ID}) of
            [Component] ->
                Component#goblet_ship_component.appearance;
            [] ->
                {error, no_such_ship_component}
        end
    end,
    mnesia:activity(transaction, Fun).
