-module(gremlin_account).

-behaviour(gremlin_rpc).

% Required callbacks for Goblet
-export([
    rpc_info/0
]).

% Handlers for protobuf messages
-export([
    new/2,
    login/2
]).

-include("gremlin_pb.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------
%% @doc Required callback for Goblet. Register the ?ACCOUNT_NEW and
%%      ?ACCOUNT_LOGIN opcodes and associated functions.
%% @end
%%----------------------------------------------------------------------------
-define(ACCOUNT_NEW, 16#0100).
-define(ACCOUNT_LOGIN, 16#0110).

-spec rpc_info() -> [{pos_integer(), mfa()}, ...].
rpc_info() ->
    [
        {?ACCOUNT_NEW, {?MODULE, new, 2}},
        {?ACCOUNT_LOGIN, {?MODULE, login, 2}}
    ].

%%----------------------------------------------------------------------------
%% @doc Registers a new account, storing it in the database
%% @end
%%----------------------------------------------------------------------------
-spec new(binary(), gremlin_session:session()) ->
    {[binary(), ...], gremlin_session:session()}.
new(Message, Session) ->
    Decode = gremlin_pb:decode_msg(Message, 'AccountNewReq'),
    Email = Decode#'AccountNewReq'.email,
    Password = Decode#'AccountNewReq'.password,
    IsValid =
        case
            gremlin_util:run_checks([
                fun() -> check_valid_email(Email) end,
                fun() -> check_valid_password(Password) end
            ])
        of
            ok -> true;
            Error -> Error
        end,
    new(Email, Password, IsValid, Session).

new(Email, Password, true, Session) ->
    {Msg, Session1} =
        case gremlin_db:create_account(Email, Password) of
            {error, Error} ->
                Reply = gremlin_protocol:response(
                    error,
                    atom_to_list(Error)
                ),
                {Reply, Session};
            _ ->
                Reply = gremlin_protocol:response(ok),
                S1 = gremlin_session:set_authenticated(true, Session),
                S2 = gremlin_session:set_email(Email, S1),
                {Reply, S2}
        end,
    OpCode = <<?ACCOUNT_NEW:16>>,
    {[OpCode, Msg], Session1};
new(_Email, _Password, {error, ErrMsg}, Session) ->
    Msg = gremlin_protocol:response(error, atom_to_list(ErrMsg)),
    OpCode = <<?ACCOUNT_NEW:16>>,
    {[OpCode, Msg], Session}.

new_test() ->
    % Generate a message mocking a new player registration
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    gremlin_db:delete_account(Email),
    Msg = gremlin_pb:encode_msg(#'AccountNewReq'{
        email = Email,
        password = Password
    }),
    {[RespOp, RespMsg], _State} = new(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, 'GenResponse'),
    ?assertEqual('OK', DecodedResp#'GenResponse'.status).

new_already_exists_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = gremlin_pb:encode_msg(#'AccountNewReq'{
        email = Email,
        password = Password
    }),
    {[RespOp, RespMsg], _State} = new(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, 'GenResponse'),
    ?assertEqual('ERROR', DecodedResp#'GenResponse'.status),
    ?assertEqual(
        "email_already_registered",
        DecodedResp#'GenResponse'.msg
    ).

%%----------------------------------------------------------------------------
%% @doc Login the user and mutate the session state
%% @end
%%----------------------------------------------------------------------------
-spec login(binary(), gremlin_session:session()) ->
    {[binary(), ...], gremlin_session:session()}.
login(Message, Session) ->
    Decode = gremlin_pb:decode_msg(Message, 'AccountNewReq'),
    Email = Decode#'AccountNewReq'.email,
    Password = Decode#'AccountNewReq'.password,
    {Msg, NewState} =
        case gremlin_db:account_login(Email, Password) of
            true ->
                Reply = gremlin_protocol:response(ok),
                S1 = gremlin_session:set_email(Email, Session),
                S2 = gremlin_session:set_authenticated(true, S1),
                {Reply, S2};
            false ->
                logger:warning("Invalid password attempt for ~p", [Email]),
                Reply = gremlin_protocol:response(
                    error,
                    "invalid password"
                ),
                {Reply, Session};
            {error, Err} ->
                logger:warning("No account exists for ~p", [Email]),
                Reply = gremlin_protocol:response(
                    error, atom_to_list(Err)
                ),
                {Reply, Session}
        end,
    OpCode = <<?ACCOUNT_LOGIN:16>>,
    {[OpCode, Msg], NewState}.

login_bad_password_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "Password",
    Msg = gremlin_pb:encode_msg(#'AccountLoginReq'{
        email = Email,
        password = Password
    }),
    {[RespOp, RespMsg], _State} = login(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, 'GenResponse'),
    ?assertEqual('ERROR', DecodedResp#'GenResponse'.status).

login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = gremlin_pb:encode_msg(#'AccountLoginReq'{
        email = Email,
        password = Password
    }),
    {[RespOp, RespMsg], _State} = login(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, 'GenResponse'),
    ?assertEqual('OK', DecodedResp#'GenResponse'.status).

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------

% TODO: Use z_stdlib email validator function? Who cares tho really.
check_valid_email([_ | _]) ->
    ok;
check_valid_email(_Email) ->
    {error, invalid_email}.

check_valid_password(Password) when length(Password) >= 8 ->
    ok;
check_valid_password(_Password) ->
    {error, password_too_short}.
