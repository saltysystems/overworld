-module(gremlin_db_account).

-include_lib("eunit/include/eunit.hrl").

-include("db/gremlin_database.hrl").

% Account stuff, probably shouldn't expose to clients
-export([
    create/2,
    delete/1,
    get_by_email/1,
    login/2,
    salt_and_hash/2
]).

-spec create(list(), list()) -> ok | {error, atom()}.
create(Email, Password) ->
    Fun = fun() ->
        case mnesia:read({gremlin_account, Email}) of
            [] ->
                NextID = mnesia:dirty_update_counter(
                    gremlin_table_ids,
                    gremlin_account,
                    1
                ),
                {Hash, Salt} = salt_and_hash(Password),
                mnesia:write(
                    gremlin_account,
                    #gremlin_account{
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

-spec delete(list()) -> ok.
delete(Email) ->
    Fun = fun() ->
        mnesia:delete({gremlin_account, Email})
    end,
    mnesia:activity(transaction, Fun).

-spec get_by_email(list()) -> tuple() | {error, atom()}.
get_by_email(Email) ->
    Fun = fun() ->
        case mnesia:read({gremlin_account, Email}) of
            [Account] ->
                Account;
            [] ->
                {error, no_such_account}
        end
    end,
    mnesia:activity(transaction, Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Account functions              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
login(Email, Password) ->
    case get_by_email(Email) of
        {error, Error} ->
            {error, Error};
        Record ->
            Hash = Record#'gremlin_account'.hash,
            Salt = Record#'gremlin_account'.salt,
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
    Hash =:= gremlin_db:salt_and_hash(Password, Salt);
valid_password(Password, Hash, Salt) ->
    Hash =:= gremlin_db:salt_and_hash(binary:list_to_bin(Password), Salt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_get_account_test() ->
    % Create an account
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Resp = gremlin_db:create(Email, Password),
    ?assertEqual(ok, Resp),

    % Get the account by name
    Resp1 = gremlin_db:get_by_email(Email),
    ?assertEqual(Email, Resp1#gremlin_account.email),
    ?assert(Resp1#gremlin_account.id > 0),

    % Try to make an account that already exists
    RespAgain = gremlin_db:create(Email, Password),
    ?assertEqual({error, email_already_registered}, RespAgain).

login_fake_test() ->
    Email = "nobody@notadomain",
    Password = "TestPassword1234",
    Resp = gremlin_db:login(Email, Password),
    ?assertEqual({error, no_such_account}, Resp).

login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Resp = gremlin_db:login(Email, Password),
    ?assertEqual(true, Resp).

login_badpass_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "thewrongpassword",
    Resp = gremlin_db:login(Email, Password),
    ?assertEqual(false, Resp).

delete_test() ->
    % Delete an account
    Email = "TestUser@doesntexist.notadomain",
    Resp = gremlin_db:delete(Email),
    ?assertEqual(ok, Resp),
    % should always succeed when deleting
    RespAgain = gremlin_db:delete(Email),
    ?assertEqual(ok, RespAgain).

get_by_email_no_acct_test() ->
    % Try to get the record for an account that doesn't exist
    Email = "TestUser@doesntexist.notadomain",
    Resp = gremlin_db:get_by_email(Email),
    ?assertEqual({error, no_such_account}, Resp).
