-module(goblet_db_tests).
-include_lib("eunit/include/eunit.hrl").

-record(goblet_account,{email, id, hash, salt, first_login, last_login, player_ids}).

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
	Title = "Fleet Captain",
	Appearance = 1,
	Role = "Destroyer",
	Resp = goblet_db:create_player(Name, Title, Appearance, Role, Email),
	?assertEqual(ok, Resp),
	% Now check to see that the player is properly associated with the 
	Acct = goblet_db:account_by_email(Email),
	?assertEqual(["Chester McTester"],Acct#goblet_account.player_ids).


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
