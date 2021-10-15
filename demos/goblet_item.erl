-module(goblet_item).

%-include_lib("eunit/include/eunit.hrl").
%
%-include("db/goblet_database.hrl").
%
%% Functions exported to the scripting interface
%-export([
%    create_item/9,
%    delete_item/1,
%    get_item/1,
%    item_to_action/1,
%    item_to_player/2,
%    item_from_player/2
%]).
%
%% Functions unexposed in the Scripting API
%-export([
%    get_all_item_owners/1
%]).
%
%-spec create_item(
%    list(),
%    pos_integer(),
%    atom(),
%    atom(),
%    non_neg_integer(),
%    non_neg_integer(),
%    atom(),
%    list(),
%    pos_integer()
%) -> ok | {error, atom()}.
%create_item(
%    Name,
%    AP,
%    Action,
%    TargetType,
%    TargetDamage,
%    TargetHealth,
%    StatusEffect,
%    Flags,
%    Price
%) ->
%    Fun = fun() ->
%        case mnesia:read({goblet_item, Name}) of
%            [] ->
%                NextID = mnesia:dirty_update_counter(
%                    goblet_table_ids,
%                    goblet_item,
%                    1
%                ),
%                mnesia:write(
%                    goblet_item,
%                    #goblet_item{
%                        name = Name,
%                        id = NextID,
%                        ap = AP,
%                        action = Action,
%                        target_type = TargetType,
%                        target_damage = TargetDamage,
%                        target_health = TargetHealth,
%                        status_effect = StatusEffect,
%                        flags = Flags,
%                        price = Price
%                    },
%                    write
%                );
%            _ ->
%                {error, item_already_exists}
%        end
%    end,
%    mnesia:activity(transaction, Fun).
%
%-spec delete_item(list()) -> ok.
%delete_item(Item) ->
%    Fun = fun() ->
%        % Items can't be removed simply without orphan objects in player
%        % inventories, so this is quite complex.
%        % First, check all players for a copy of the item.
%        % Remove the item from the player's inventory.
%        % Then finally delete the item itself.
%        mnesia:foldl(
%            fun(Rec, Acc) ->
%                Inventory = Rec#goblet_player.inventory,
%                case lists:member(Item, Inventory) of
%                    true ->
%                        I2 = lists:delete(Item, Inventory),
%                        mnesia:write(
%                            goblet_player,
%                            Rec#goblet_player{inventory = I2},
%                            write
%                        ),
%                        [ok | Acc];
%                    _ ->
%                        Acc
%                end
%            end,
%            [],
%            goblet_player
%        ),
%        mnesia:delete({goblet_item, Item})
%    end,
%    mnesia:activity(transaction, Fun).
%
%-spec get_all_item_owners(list()) -> list().
%get_all_item_owners(Item) ->
%    F = fun() ->
%        % Check all players for a copy of the item. If found, add it to
%        % the list. Finally, return the list back to the caller.
%        mnesia:foldl(
%            fun(Rec, Acc) ->
%                case lists:member(Item, Rec#goblet_player.inventory) of
%                    true -> [Rec#goblet_player.name | Acc];
%                    _ -> Acc
%                end
%            end,
%            [],
%            goblet_player
%        )
%    end,
%    mnesia:activity(transaction, F).
%
%-spec get_item(list()) -> tuple().
%get_item(Item) ->
%    Fun = fun() ->
%        case mnesia:read({goblet_item, Item}) of
%            [I] ->
%                I;
%            Other ->
%                Other
%        end
%    end,
%    mnesia:activity(transaction, Fun).
%
%-spec item_to_action(list()) -> {atom(), pos_integer(), list()}.
%item_to_action(Item) ->
%    Fun = fun() ->
%        [I] = mnesia:read({goblet_item, Item}),
%        % lets just crash as early as possible
%        {I#goblet_item.action, I#goblet_item.ap, I#goblet_item.flags}
%    end,
%    mnesia:activity(transaction, Fun).
%
%-spec item_to_player(list(), list()) -> ok | {error, atom()}.
%item_to_player(Item, Player) ->
%    Fun = fun() ->
%        case mnesia:read({goblet_item, Item}) of
%            [I] ->
%                ItemName = I#goblet_item.name,
%                case mnesia:read({goblet_player, Player}) of
%                    [P] ->
%                        Inventory = P#goblet_player.inventory,
%                        mnesia:write(
%                            goblet_player,
%                            P#goblet_player{
%                                inventory = [ItemName | Inventory]
%                            },
%                            write
%                        );
%                    _ ->
%                        {error, no_such_player}
%                end;
%            _ ->
%                {error, no_such_item}
%        end
%    end,
%    mnesia:activity(transaction, Fun).
%
%-spec item_from_player(list(), list()) -> ok | {error, atom()}.
%item_from_player(Item, Player) ->
%    Fun = fun() ->
%        case mnesia:read({goblet_player, Player}) of
%            [P] ->
%                Inventory = P#goblet_player.inventory,
%                I2 = lists:delete(Item, Inventory),
%                mnesia:write(
%                    goblet_player,
%                    P#goblet_player{
%                        inventory = I2
%                    },
%                    write
%                );
%            _ ->
%                {error, no_such_player}
%        end
%    end,
%    mnesia:activity(transaction, Fun).
%
%player_items_have_action(Name, Action) ->
%    Fun = fun() ->
%        I = #goblet_item{name = '$1', action = Action, _ = '_'},
%        mnesia:select(goblet_item, [{I, [], ['$1']}])
%    end,
%    Matches = mnesia:activity(transaction, Fun),
%    case goblet_db:player_by_name(Name) of
%        {error, E} ->
%            {error, E};
%        P ->
%            Items = P#goblet_player.inventory,
%            [X || X <- Items, lists:member(X, Matches) == true]
%    end.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%create_item_test() ->
%    Name = "TestGun",
%    AP = 3,
%    Action = testgun,
%    TargetType = direct,
%    TargetDamage = 10,
%    TargetHealth = 0,
%    StatusEffect = none,
%    Flags = [],
%    Price = 1000,
%    Resp = goblet_db:create_item(
%        Name,
%        AP,
%        Action,
%        TargetType,
%        TargetDamage,
%        TargetHealth,
%        StatusEffect,
%        Flags,
%        Price
%    ),
%    ?assertEqual(ok, Resp).
%
%item_to_player_test() ->
%    Item = "TestGun",
%    Player = "Chester McTester",
%    Resp = goblet_db:item_to_player(Item, Player),
%    ?assertEqual(ok, Resp),
%    P = goblet_db:player_by_name(Player),
%    Inv = P#goblet_player.inventory,
%    ?assertEqual(true, lists:member(Item, Inv)).
%
%player_items_have_action_test() ->
%    Player = "Chester McTester",
%    Resp = goblet_db:player_items_have_action(Player, testgun),
%    ?assertEqual(["TestGun"], Resp).
%
%get_all_item_owners_test() ->
%    Player = "Chester McTester",
%    Item = "TestGun",
%    [Resp] = get_all_item_owners(Item),
%    ?assertEqual(Resp, Player).
%
%delete_item_test() ->
%    Item = "TestGun",
%    Resp1 = goblet_db:delete_item(Item),
%    ?assertEqual(ok, Resp1),
%    P = "Chester McTester",
%    Resp2 = goblet_db:player_inventory(P),
%    ItemExists = lists:member(Item, Resp2),
%    ?assertEqual(false, ItemExists).
