-module(gremlin_account).

-behaviour(gremlin_rpc).

% Required callbacks for Goblet
-export([
    rpc_info/0
]).

% Handlers for protobuf messages
-export([
    new_account/2,
    login/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------
%% @doc Required callback for Goblet. Register the ?ACCOUNT_NEW and
%%      ?ACCOUNT_LOGIN opcodes, callbacks and messages.
%% @end
%%----------------------------------------------------------------------------

-define(ACCOUNT_NEW, 16#0100).
-define(ACCOUNT_LOGIN, 16#0110).

-spec rpc_info() -> gremlin_rpc:callbacks().
rpc_info() ->
    [
        #{
            opcode => ?ACCOUNT_NEW,
            c2s_handler => {?MODULE, new_account, 2},
            s2c_call => gen_response,
            encoder => gremlin_pb
        },
        #{
            opcode => ?ACCOUNT_LOGIN,
            c2s_handler => {?MODULE, login, 2},
            s2c_call => gen_response,
            encoder => gremlin_pb
        }
    ].

%%----------------------------------------------------------------------------
%% @doc Registers a new account, storing it in the database
%% @end
%%----------------------------------------------------------------------------
-spec new_account(binary(), gremlin_session:session()) ->
    gremlin_session:net_msg().
new_account(Message, Session) ->
    Decode = gremlin_pb:decode_msg(Message, new_account),
    Email = maps:get(email, Decode),
    Password = maps:get(password, Decode),
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
    new_account(Email, Password, IsValid, Session).

new_account(Email, Password, true, Session) ->
    {Msg, Session1} =
        case gremlin_db_account:create(Email, Password) of
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
    R = {[OpCode, Msg], Session1},
    io:format("Response: ~p~n", [R]),
    R;
new_account(_Email, _Password, {error, ErrMsg}, Session) ->
    Msg = gremlin_protocol:response(error, atom_to_list(ErrMsg)),
    OpCode = <<?ACCOUNT_NEW:16>>,
    R = {[OpCode, Msg], Session},
    io:format("Response: ~p~n", [R]),
    R.

new_test() ->
    % Generate a message mocking a new player registration
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    gremlin_db_account:delete(Email),
    Msg = gremlin_pb:encode_msg(
        #{
            email => Email,
            password => Password
        },
        new_account
    ),
    {[RespOp, RespMsg], _State} = new_account(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, gen_response),
    ?assertEqual('OK', maps:get(status, DecodedResp)).

new_already_exists_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = gremlin_pb:encode_msg(
        #{
            email => Email,
            password => Password
        },
        new_account
    ),
    {[RespOp, RespMsg], _State} = new_account(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, gen_response),
    ?assertEqual('ERROR', maps:get(status, DecodedResp)),
    ?assertEqual("email_already_registered", maps:get(msg, DecodedResp)).

%%----------------------------------------------------------------------------
%% @doc Login the user and mutate the session state
%% @end
%%----------------------------------------------------------------------------
-spec login(binary(), gremlin_session:session()) ->
    gremlin_session:net_msg().
login(Message, Session) ->
    Decode = gremlin_pb:decode_msg(Message, account_login),
    Email = maps:get(email, Decode),
    Password = maps:get(password, Decode),
    {Msg, NewState} =
        case gremlin_db_account:login(Email, Password) of
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
    Msg = gremlin_pb:encode_msg(
        #{
            email => Email,
            password => Password
        },
        account_login
    ),
    {[RespOp, RespMsg], _State} = login(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, gen_response),
    ?assertEqual('ERROR', maps:get(status, DecodedResp)).

login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = gremlin_pb:encode_msg(
        #{
            email => Email,
            password => Password
        },
        account_login
    ),
    {[RespOp, RespMsg], _State} = login(Msg, gremlin_session:new()),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = gremlin_pb:decode_msg(RespMsg, gen_response),
    ?assertEqual('OK', maps:get(status, DecodedResp)).

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
