-module(goblet_db).

-include_lib("eunit/include/eunit.hrl").

-include("db/goblet_database.hrl").

-export([
    create_account/2,
    delete_account/1,
    account_by_email/1,
    account_login/2,
    create_player/2,
    create_mob/2,
    delete_mob/1,
    mob_instance/2,
    mob_by_name/1,
    delete_player/2,
    delete_orphaned_player/1,
    player_by_name/1,
    player_inventory/1,
    player_items_have_action/2,
    player_shadow/1,
    player_unshadow/3,
    is_valid_player/1,
    is_valid_player_account/2,
    %is_player_alive/1,
    %update_player_health/2,
    create_item/9,
    delete_item/1,
    get_item/1,
    item_to_action/1,
    item_to_player/2,
    item_from_player/2,
    get_all_item_owners/1,
    salt_and_hash/2
]).

-spec create_account(list(), list()) -> ok | {error, atom()}.
create_account(Email, Password) ->
    Fun = fun() ->
        case mnesia:read({goblet_account, Email}) of
            [] ->
                NextID = mnesia:dirty_update_counter(
                    goblet_table_ids,
                    goblet_account,
                    1
                ),
                {Hash, Salt} = salt_and_hash(Password),
                mnesia:write(
                    goblet_account,
                    #goblet_account{
                        email = Email,
                        id = NextID,
                        hash = Hash,
                        salt = Salt,
                        first_login = os:system_time(),
                        last_login = os:system_time(),
                        player_ids = []
                    },
                    write
                );
            _ ->
                {error, email_already_registered}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec delete_account(list()) -> ok.
delete_account(Email) ->
    Fun = fun() ->
        mnesia:delete({goblet_account, Email})
    end,
    mnesia:activity(transaction, Fun).

-spec account_by_email(list()) -> tuple() | {error, atom()}.
account_by_email(Email) ->
    Fun = fun() ->
        case mnesia:read({goblet_account, Email}) of
            [Account] ->
                Account;
            [] ->
                {error, no_such_account}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec mob_instance(list(), pos_integer()) -> tuple() | {error, atom()}.
mob_instance(Name, UniqueID) ->
    case mob_by_name(Name) of
        {error, _} ->
            [];
        M ->
            {
                io_lib:format("~s_~p", [
                    M#goblet_mob.name,
                    UniqueID
                ])
                %M#goblet_mob.appearance,
                %M#goblet_mob.type,
                %M#goblet_mob.health,
                %M#goblet_mob.energy,
                %M#goblet_mob.flags,
                %M#goblet_mob.inventory
            }
    end.

-spec mob_by_name(list()) -> tuple() | {error, atom()}.
mob_by_name(Name) ->
    Fun = fun() ->
        case mnesia:read({goblet_mob, Name}) of
            [Mob] ->
                Mob;
            [] ->
                {error, no_such_mob}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec create_mob(
    list(),
    list()
) -> ok | {error, atom()}.
create_mob(Name, Inventory) ->
    Fun = fun() ->
        case mnesia:read({goblet_mob, Name}) of
            [] ->
                NextID = mnesia:dirty_update_counter(
                    goblet_table_ids,
                    goblet_mob,
                    1
                ),
                mnesia:write(
                    goblet_mob,
                    #goblet_mob{
                        name = Name,
                        id = NextID,
                        inventory = Inventory
                    },
                    write
                );
            _ ->
                {error, mob_already_exists}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec delete_mob(list()) -> ok.
delete_mob(Name) ->
    Fun = fun() ->
        mnesia:delete({goblet_mob, Name})
    end,
    mnesia:activity(transaction, Fun).

-spec player_by_name(list()) -> tuple() | {error, atom()}.
player_by_name(Name) ->
    Fun = fun() ->
        case mnesia:read({goblet_player, Name}) of
            [Player] ->
                Player;
            [] ->
                {error, no_such_player}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec player_shadow(list()) -> tuple() | {error, atom()}.
player_shadow(Name) ->
    case player_by_name(Name) of
        {error, _} ->
            [];
        P ->
            {
                P#goblet_player.name,
                P#goblet_player.flags,
                P#goblet_player.inventory
            }
    end.

-spec player_unshadow(
    list(),
    list(),
    list()
) -> ok | {error, atom()}.
player_unshadow(Name, Flags, Inventory) ->
    Fun = fun() ->
        case mnesia:read({goblet_player, Name}) of
            [] ->
                {error, no_such_player};
            [Player] ->
                mnesia:write(
                    goblet_player,
                    Player#goblet_player{
                        flags = Flags,
                        inventory = Inventory
                    },
                    write
                )
        end
    end,
    mnesia:activity(transaction, Fun).

-spec player_inventory(list()) -> list() | {error, atom()}.
player_inventory(Name) ->
    case player_by_name(Name) of
        {error, _} -> [];
        P -> P#goblet_player.inventory
    end.

-spec is_valid_player(list()) -> true | false.
is_valid_player(Name) ->
    case player_by_name(Name) of
        {error, _} -> false;
        _Player -> true
    end.

-spec is_valid_player_account(list(), list()) -> true | false.
is_valid_player_account(Player, Email) ->
    case account_by_email(Email) of
        {error, _} ->
            % account doesn't exist at all
            false;
        % check validity
        Account ->
            lists:member(Player, Account#goblet_account.player_ids)
    end.

%-spec is_player_alive(list()) -> true | false | {error, any()}.
%is_player_alive(Name) ->
%    case player_by_name(Name) of
%        {error, E} ->
%            {error, E};
%        Player ->
%            % we can get fancy with components and such later.
%            Player#goblet_player.health >= 0
%    end.

-spec create_player(list(), list()) ->
    ok | {error, atom()}.
create_player(Name, Account) ->
    Fun = fun() ->
        case mnesia:read({goblet_player, Name}) of
            [] ->
                NextID = mnesia:dirty_update_counter(
                    goblet_table_ids,
                    goblet_player,
                    1
                ),
                mnesia:write(
                    goblet_player,
                    #goblet_player{
                        name = Name,
                        id = NextID,
                        flags = [],
                        inventory = [],
                        shipgrid = #{}
                    },
                    write
                ),
                % Get the account information
                [Resp] = mnesia:read({goblet_account, Account}),
                Players = Resp#goblet_account.player_ids,
                mnesia:write(
                    goblet_account,
                    Resp#goblet_account{player_ids = [Name | Players]},
                    write
                );
            _ ->
                {error, player_already_exists}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec delete_player(list(), list()) -> ok.
delete_player(Name, Account) ->
    Fun = fun() ->
        mnesia:delete({goblet_player, Name}),
        [Resp] = mnesia:read({goblet_account, Account}),
        Players = Resp#goblet_account.player_ids,
        mnesia:write(
            goblet_account,
            Resp#goblet_account{player_ids = lists:delete(Name, Players)},
            write
        )
    end,
    mnesia:activity(transaction, Fun).

%-spec update_player_health(list(), integer()) -> ok.
%update_player_health(Name, Value) ->
%    Fun = fun() ->
%        case mnesia:read({goblet_player, Name}) of
%            [] ->
%                {error, no_such_player};
%            [Player] ->
%                mnesia:write(
%                    goblet_player,
%                    Player#goblet_player{health = Value},
%                    write
%                )
%        end
%    end,
%    mnesia:activity(transaction, Fun).

-spec create_item(
    list(),
    pos_integer(),
    atom(),
    atom(),
    non_neg_integer(),
    non_neg_integer(),
    atom(),
    list(),
    pos_integer()
) -> ok | {error, atom()}.
create_item(
    Name,
    AP,
    Action,
    TargetType,
    TargetDamage,
    TargetHealth,
    StatusEffect,
    Flags,
    Price
) ->
    Fun = fun() ->
        case mnesia:read({goblet_item, Name}) of
            [] ->
                NextID = mnesia:dirty_update_counter(
                    goblet_table_ids,
                    goblet_item,
                    1
                ),
                mnesia:write(
                    goblet_item,
                    #goblet_item{
                        name = Name,
                        id = NextID,
                        ap = AP,
                        action = Action,
                        target_type = TargetType,
                        target_damage = TargetDamage,
                        target_health = TargetHealth,
                        status_effect = StatusEffect,
                        flags = Flags,
                        price = Price
                    },
                    write
                );
            _ ->
                {error, item_already_exists}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec delete_item(list()) -> ok.
delete_item(Item) ->
    Fun = fun() ->
        % Items can't be removed simply without orphan objects in player
        % inventories, so this is quite complex.
        % First, check all players for a copy of the item.
        % Remove the item from the player's inventory.
        % Then finally delete the item itself.
        mnesia:foldl(
            fun(Rec, Acc) ->
                Inventory = Rec#goblet_player.inventory,
                case lists:member(Item, Inventory) of
                    true ->
                        I2 = lists:delete(Item, Inventory),
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
        mnesia:delete({goblet_item, Item})
    end,
    mnesia:activity(transaction, Fun).

-spec get_all_item_owners(list()) -> list().
get_all_item_owners(Item) ->
    F = fun() ->
        % Check all players for a copy of the item. If found, add it to
        % the list. Finally, return the list back to the caller.
        mnesia:foldl(
            fun(Rec, Acc) ->
                case lists:member(Item, Rec#goblet_player.inventory) of
                    true -> [Rec#goblet_player.name | Acc];
                    _ -> Acc
                end
            end,
            [],
            goblet_player
        )
    end,
    mnesia:activity(transaction, F).

-spec get_item(list()) -> tuple().
get_item(Item) ->
    Fun = fun() ->
        case mnesia:read({goblet_item, Item}) of
            [I] ->
                I;
            Other ->
                Other
        end
    end,
    mnesia:activity(transaction, Fun).

-spec item_to_action(list()) -> {atom(), pos_integer(), list()}.
item_to_action(Item) ->
    Fun = fun() ->
        [I] = mnesia:read({goblet_item, Item}),
        % lets just crash as early as possible
        {I#goblet_item.action, I#goblet_item.ap, I#goblet_item.flags}
    end,
    mnesia:activity(transaction, Fun).

-spec item_to_player(list(), list()) -> ok | {error, atom()}.
item_to_player(Item, Player) ->
    Fun = fun() ->
        case mnesia:read({goblet_item, Item}) of
            [I] ->
                ItemName = I#goblet_item.name,
                case mnesia:read({goblet_player, Player}) of
                    [P] ->
                        Inventory = P#goblet_player.inventory,
                        mnesia:write(
                            goblet_player,
                            P#goblet_player{
                                inventory = [ItemName | Inventory]
                            },
                            write
                        );
                    _ ->
                        {error, no_such_player}
                end;
            _ ->
                {error, no_such_item}
        end
    end,
    mnesia:activity(transaction, Fun).

-spec item_from_player(list(), list()) -> ok | {error, atom()}.
item_from_player(Item, Player) ->
    Fun = fun() ->
        case mnesia:read({goblet_player, Player}) of
            [P] ->
                Inventory = P#goblet_player.inventory,
                I2 = lists:delete(Item, Inventory),
                mnesia:write(
                    goblet_player,
                    P#goblet_player{
                        inventory = I2
                    },
                    write
                );
            _ ->
                {error, no_such_player}
        end
    end,
    mnesia:activity(transaction, Fun).

player_items_have_action(Name, Action) ->
    Fun = fun() ->
        I = #goblet_item{name = '$1', action = Action, _ = '_'},
        mnesia:select(goblet_item, [{I, [], ['$1']}])
    end,
    Matches = mnesia:activity(transaction, Fun),
    case player_by_name(Name) of
        {error, E} ->
            {error, E};
        P ->
            Items = P#goblet_player.inventory,
            [X || X <- Items, lists:member(X, Matches) == true]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exported, but unsafe functions %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec delete_orphaned_player(list()) -> ok.
delete_orphaned_player(Name) ->
    % Some players can get orphaned from their account during unusual
    % conditions or database testing
    Fun = fun() ->
        mnesia:delete({goblet_player, Name})
    end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Account functions              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
account_login(Email, Password) ->
    case account_by_email(Email) of
        {error, Error} ->
            {error, Error};
        Record ->
            Hash = Record#'goblet_account'.hash,
            Salt = Record#'goblet_account'.salt,
            valid_password(Password, Hash, Salt)
    end.

salt_and_hash(Password) when is_binary(Password) ->
    Salt = crypto:strong_rand_bytes(32),
    Hash = crypto:hash(sha256, <<Password/binary, Salt/binary>>),
    {Hash, Salt};
salt_and_hash(Password) ->
    Salt = crypto:strong_rand_bytes(32),
    BinaryPassword = binary:list_to_bin(Password),
    Hash = crypto:hash(sha256, <<BinaryPassword/binary, Salt/binary>>),
    {Hash, Salt}.

salt_and_hash(Password, Salt) when is_binary(Password) ->
    crypto:hash(sha256, <<Password/binary, Salt/binary>>);
salt_and_hash(Password, Salt) ->
    BinaryPassword = binary:list_to_bin(Password),
    crypto:hash(sha256, <<BinaryPassword/binary, Salt/binary>>).

valid_password(Password, Hash, Salt) when is_binary(Password) ->
    Hash =:= goblet_db:salt_and_hash(Password, Salt);
valid_password(Password, Hash, Salt) ->
    Hash =:= goblet_db:salt_and_hash(binary:list_to_bin(Password), Salt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_get_account_test() ->
    % Create an account
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Resp = goblet_db:create_account(Email, Password),
    ?assertEqual(ok, Resp),

    % Get the account by name
    Resp1 = goblet_db:account_by_email(Email),
    ?assertEqual(Email, Resp1#goblet_account.email),
    ?assert(Resp1#goblet_account.id > 0),

    % Try to make an account that already exists
    RespAgain = goblet_db:create_account(Email, Password),
    ?assertEqual({error, email_already_registered}, RespAgain).

account_login_fake_test() ->
    Email = "nobody@notadomain",
    Password = "TestPassword1234",
    Resp = goblet_db:account_login(Email, Password),
    ?assertEqual({error, no_such_account}, Resp).

account_login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Resp = goblet_db:account_login(Email, Password),
    ?assertEqual(true, Resp).

account_login_badpass_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "thewrongpassword",
    Resp = goblet_db:account_login(Email, Password),
    ?assertEqual(false, Resp).

create_player_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Name = "Chester McTester",
    Colors = ["#f00d00", "#ffffff", "#deabee"],
    Symbols = [1, 3],
    Role = 'DESTROYER',
    Resp = goblet_db:create_player(Name, Colors, Symbols, Role, Email),
    ?assertEqual(ok, Resp),
    % Now check to see that the player is properly associated with the
    Acct = goblet_db:account_by_email(Email),
    ?assertEqual(["Chester McTester"], Acct#goblet_account.player_ids).

create_item_test() ->
    Name = "TestGun",
    AP = 3,
    Action = testgun,
    TargetType = direct,
    TargetDamage = 10,
    TargetHealth = 0,
    StatusEffect = none,
    Flags = [],
    Price = 1000,
    Resp = goblet_db:create_item(
        Name,
        AP,
        Action,
        TargetType,
        TargetDamage,
        TargetHealth,
        StatusEffect,
        Flags,
        Price
    ),
    ?assertEqual(ok, Resp).

create_delete_mob_test() ->
    Name = "Space Bug",
    Appearance = 0,
    Type = bug,
    Health = 100,
    Energy = 100,
    Inventory = [],
    Resp = goblet_db:create_mob(
        Name,
        Appearance,
        Type,
        Health,
        Energy,
        Inventory
    ),
    ?assertEqual(ok, Resp),
    Mob = goblet_db:mob_by_name(Name),
    ?assertEqual(Mob#goblet_mob.inventory, Inventory),
    Resp1 = goblet_db:delete_mob(Name),
    ?assertEqual(ok, Resp1).

item_to_player_test() ->
    Item = "TestGun",
    Player = "Chester McTester",
    Resp = goblet_db:item_to_player(Item, Player),
    ?assertEqual(ok, Resp),
    P = goblet_db:player_by_name(Player),
    Inv = P#goblet_player.inventory,
    ?assertEqual(true, lists:member(Item, Inv)).

player_items_have_action_test() ->
    Player = "Chester McTester",
    Resp = goblet_db:player_items_have_action(Player, testgun),
    ?assertEqual(["TestGun"], Resp).

get_all_item_owners_test() ->
    Player = "Chester McTester",
    Item = "TestGun",
    [Resp] = get_all_item_owners(Item),
    ?assertEqual(Resp, Player).

delete_item_test() ->
    Item = "TestGun",
    Resp1 = goblet_db:delete_item(Item),
    ?assertEqual(ok, Resp1),
    P = "Chester McTester",
    Resp2 = goblet_db:player_inventory(P),
    ItemExists = lists:member(Item, Resp2),
    ?assertEqual(false, ItemExists).

delete_player_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Name = "Chester McTester",
    Resp = goblet_db:delete_player(Name, Email),
    ?assertEqual(ok, Resp).

delete_account_test() ->
    % Delete an account
    Email = "TestUser@doesntexist.notadomain",
    Resp = goblet_db:delete_account(Email),
    ?assertEqual(ok, Resp),
    % should always succeed when deleting
    RespAgain = goblet_db:delete_account(Email),
    ?assertEqual(ok, RespAgain).

account_by_email_no_acct_test() ->
    % Try to get the record for an account that doesn't exist
    Email = "TestUser@doesntexist.notadomain",
    Resp = goblet_db:account_by_email(Email),
    ?assertEqual({error, no_such_account}, Resp).
