-module(ow_account).

% Handlers for protobuf messages
-export([
    new_account/2,
    login/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------
%% @doc RPC hints for generating client library
%% @end
%%----------------------------------------------------------------------------

-rpc_encoder(overworld_pb).
-rpc_server([account_new, account_login]).
-rpc_client([gen_response]).

%%----------------------------------------------------------------------------
%% @doc Registers a new account, storing it in the database
%% @end
%%----------------------------------------------------------------------------
-spec new_account(map(), ow_session:session()) ->
    {binary(), ow_session:session()}.
new_account(#{email := Email, password := Password}, Session) ->
    IsValid =
        case
            ow_util:run_checks([
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
        case ow_db_account:create(Email, Password) of
            {error, Error} ->
                Reply = ow_protocol:response(
                    error,
                    atom_to_list(Error)
                ),
                {Reply, Session};
            _ ->
                Reply = ow_protocol:response(ok),
                S1 = ow_session:set_authenticated(true, Session),
                {Reply, S1}
        end,
    {Msg, Session1};
new_account(_Email, _Password, {error, ErrMsg}, Session) ->
    Msg = ow_protocol:response(error, atom_to_list(ErrMsg)),
    {Msg, Session}.

new_test() ->
    % Generate a message mocking a new player registration
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    ow_db_account:delete(Email),
    Msg = #{
        email => Email,
        password => Password
    },
    {RespMsg, _State} = new_account(Msg, ow_session:new()),
    {_Type, DecodedResp} = ow_msg:raw_decode(RespMsg),
    ?assertEqual('OK', maps:get(status, DecodedResp)).

new_already_exists_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = #{
        email => Email,
        password => Password
    },
    {RespMsg, _State} = new_account(Msg, ow_session:new()),
    {_Type, DecodedResp} = ow_msg:raw_decode(RespMsg),
    ?assertEqual('ERROR', maps:get(status, DecodedResp)),
    ?assertEqual("email_already_registered", maps:get(msg, DecodedResp)).

%%----------------------------------------------------------------------------
%% @doc Login the user and mutate the session state
%% @end
%%----------------------------------------------------------------------------
-spec login(map(), ow_session:session()) ->
    {binary(), ow_session:session()}.
login(#{email := Email, password := Password}, Session) ->
    {Msg, NewState} =
        case ow_db_account:login(Email, Password) of
            true ->
                Reply = ow_protocol:response(ok),
                S1 = ow_session:set_authenticated(true, Session),
                {Reply, S1};
            false ->
                logger:warning("Invalid password attempt for ~p", [Email]),
                Reply = ow_protocol:response(
                    error,
                    "invalid password"
                ),
                {Reply, Session};
            {error, Err} ->
                logger:warning("No account exists for ~p", [Email]),
                Reply = ow_protocol:response(
                    error, atom_to_list(Err)
                ),
                {Reply, Session}
        end,
    {Msg, NewState}.

login_bad_password_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "Password",
    Msg = #{
        email => Email,
        password => Password
    },
    {RespMsg, _Session} = login(Msg, ow_session:new()),
    {_Type, DecodedResp} = ow_msg:raw_decode(RespMsg),
    ?assertEqual('ERROR', maps:get(status, DecodedResp)).

login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = #{
        email => Email,
        password => Password
    },
    {RespMsg, _State} = login(Msg, ow_session:new()),
    {_Type, DecodedResp} = ow_msg:raw_decode(RespMsg),
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
