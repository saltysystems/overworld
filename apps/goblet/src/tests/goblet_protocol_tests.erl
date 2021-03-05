-module(goblet_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

-include("goblet_pb.hrl").
-include("goblet_opcode.hrl").

-record(session, {email=none, authenticated=false, player=none}).

player_log_test() ->
	OriginalMessage = "Hello World",
	[OpCode, Message] = goblet_protocol:player_log(OriginalMessage),
	% Check that the expected OpCode comes back
	?assertEqual(OpCode, <<?PLAYER_LOG:16>>),
	% Check that the message decodes correctly
	Decoded = goblet_pb:decode_msg(Message, 'PlayerLog'),
	ToList = binary:bin_to_list(Decoded#'PlayerLog'.msg),
	?assertEqual(OriginalMessage, ToList),
	ok.


account_new_test() ->
	% Generate a message mocking a new player registration
	Email = "TestUser@doesntexist.notadomain",
	Password = "TestPassword1234",
	goblet_db:delete_account(Email),
	Msg = goblet_pb:encode_msg(#'AccountNewReq'{email=Email, password=Password}),
	{[RespOp, RespMsg], _State} = goblet_protocol:account_new(Msg),
	?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
	DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountNewResp'),
	?assertEqual('OK', DecodedResp#'AccountNewResp'.status).

account_new_already_exists_test() ->
	Email = "TestUser@doesntexist.notadomain",
	Password = "TestPassword1234",
	Msg = goblet_pb:encode_msg(#'AccountNewReq'{email=Email, password=Password}),
	{[RespOp, RespMsg], _State} = goblet_protocol:account_new(Msg),
	?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
	DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountNewResp'),
	?assertEqual('ERROR', DecodedResp#'AccountNewResp'.status),
	?assertEqual(<<"email_already_registered">>, DecodedResp#'AccountNewResp'.error).


account_login_bad_password_test() ->
	Email = "TestUser@doesntexist.notadomain",
	Password = "Password",
	Msg = goblet_pb:encode_msg(#'AccountLoginReq'{email=Email, password=Password}),
	{[RespOp, RespMsg], _State} = goblet_protocol:account_login(Msg),
	?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
	DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountLoginResp'),
	?assertEqual('ERROR', DecodedResp#'AccountLoginResp'.status).

account_login_test() ->
	Email = "TestUser@doesntexist.notadomain",
	Password = "TestPassword1234",
	Msg = goblet_pb:encode_msg(#'AccountLoginReq'{email=Email, password=Password}),
	{[RespOp, RespMsg], _State} = goblet_protocol:account_login(Msg),
	?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
	DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountLoginResp'),
	?assertEqual('OK', DecodedResp#'AccountLoginResp'.status),
	?assertEqual([], DecodedResp#'AccountLoginResp'.players).

player_new_test() ->
	% mock the state
	Email = "TestUser@doesntexist.notadomain",
	State = #session{email=Email, authenticated=true},
	Message = goblet_pb:encode_msg(#'PlayerNewReq'{
				name = "Chester The Tester",
				title = "Rickety Mockup",
				appearance = 1,
				role = 'INTERCEPTOR'}),
	goblet_protocol:player_new(Message, State).
