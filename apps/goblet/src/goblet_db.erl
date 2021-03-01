-module(goblet_db).

-include("goblet_database.hrl").

-export([
    create_account/2,
    delete_account/1,
    account_by_email/1,
    account_login/2,
    create_player/6,
    delete_player/2,
    player_by_name/1,
    salt_and_hash/2
]).

create_account(Email, Password) ->
    Fun = fun() ->
        case mnesia:read({goblet_account, Email}) of
            [] ->
                NextID = mnesia:dirty_update_counter(goblet_table_ids, goblet_account, 1),
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

delete_account(Email) ->
    Fun = fun() ->
        mnesia:delete({goblet_account, Email})
    end,
    mnesia:activity(transaction, Fun).

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

create_player(Name, Title, Appearance, Role, Zone, Email) ->
    Fun = fun() ->
        case mnesia:read({goblet_player, Name}) of
            [] ->
                NextID = mnesia:dirty_update_counter(goblet_table_ids, goblet_player, 1),
                mnesia:write(
                    goblet_player,
                    #goblet_player{
                        name = Name,
                        id = NextID,
                        title = Title,
                        appearance = Appearance,
                        role = Role,
                        stats = [],
                        inventory = [],
                        zone = Zone,
                        online = false
                    },
                    write
                ),
                % Get the account information
                [Account] = mnesia:read({goblet_account, Email}),
                Players = Account#goblet_account.player_ids,
                mnesia:write(
                    goblet_account,
                    Account#goblet_account{player_ids = [Name | Players]},
                    write
                );
            _ ->
                {error, player_already_exists}
        end
    end,
    mnesia:activity(transaction, Fun).

delete_player(Name, Email) ->
    Fun = fun() ->
        mnesia:delete({goblet_player, Name}),
        [Account] = mnesia:read({goblet_account, Email}),
        Players = Account#goblet_account.player_ids,
        mnesia:write(
            goblet_account,
            Account#goblet_account{player_ids = lists:delete(Name, Players)},
            write
        )
    end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

account_login(Email, Password) ->
    case account_by_email(Email) of
        [] ->
            {error, no_such_account};
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
