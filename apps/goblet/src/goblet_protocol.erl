-module(goblet_protocol).

-export([decode/2, account_new/1, account_login/1, player_new/2]).
-export([player_log/1]).

-include("goblet_opcode.hrl").
-include("goblet_pb.hrl").
-include("goblet_database.hrl").

-include_lib("kernel/include/logger.hrl").

-record(session, {email = none, authenticated = false, player = none}).

%%============================================================================
%% Goblet Protocol.
%%
%% This module handles decoding serialized messages and routing them to the
%% correct place for further processing and/or replies.
%%
%%============================================================================

%%----------------------------------------------------------------------------
%% @doc Decode messages from clients and route them to an appropriate function
%% @end
%%----------------------------------------------------------------------------
-spec decode(binary(), any()) -> {ok, any()} | {binary(), any()}.
decode(<<OpCode:16, _T/binary>>, State) when OpCode =:= ?VERSION ->
    logger:notice("Got a version request~n"),
    {ok, State};
decode(<<OpCode:16, T/binary>>, _State) when OpCode =:= ?ACCOUNT_NEW ->
    logger:notice("Got a new account request~n"),
    account_new(T);
decode(<<OpCode:16, T/binary>>, _State) when OpCode =:= ?ACCOUNT_LOGIN ->
    logger:notice("Got an account login request~n"),
    account_login(T);
decode(<<OpCode:16, T/binary>>, State) when OpCode =:= ?PLAYER_NEW ->
    logger:notice("Got a new player request~n"),
    % Need to inspect the state for acct info
    player_new(T, State);
decode(<<OpCode:16, _T/binary>>, State) ->
    logger:notice("Got an unknown opcode: ~p", [OpCode]),
    {ok, State}.

%%----------------------------------------------------------------------------
%% @doc Encodes a log message to be sent back to the client
%% @end
%%----------------------------------------------------------------------------
-spec player_log(list()) -> [binary(), ...].
player_log(Message) ->
    OpCode = <<?PLAYER_LOG:16>>,
    Sanitized = sanitize_message(Message),
    Msg = goblet_pb:encode_msg(#'PlayerLog'{msg = Sanitized}),
    [OpCode, Msg].

%%----------------------------------------------------------------------------
%% @doc Registers a new account, storing it in the database
%% @end
%%----------------------------------------------------------------------------
-spec account_new(binary()) -> {[binary(), ...], any()}.
account_new(Message) ->
    Decode = goblet_pb:decode_msg(Message, 'AccountNewReq'),
    Email = Decode#'AccountNewReq'.email,
    Password = Decode#'AccountNewReq'.password,
    % TODO Validate length
    {Msg, NewState} =
        case goblet_db:create_account(binary:bin_to_list(Email), Password) of
            {error, Error} ->
                Reply = goblet_pb:encode_msg(#'AccountNewResp'{
                    status = 'ERROR',
                    error = atom_to_list(Error)
                }),
                {Reply, #session{authenticated = false}};
            _ ->
                Reply = goblet_pb:encode_msg(#'AccountNewResp'{status = 'OK'}),
                {Reply, #session{email = Email, authenticated = true}}
        end,
    OpCode = <<?ACCOUNT_NEW:16>>,
    {[OpCode, Msg], NewState}.

%%----------------------------------------------------------------------------
%% @doc Login the user and mutate the session state
%% @end
%%----------------------------------------------------------------------------
-spec account_login(binary()) -> {[binary(), ...], any()}.
account_login(Message) ->
    Decode = goblet_pb:decode_msg(Message, 'AccountNewReq'),
    Email = binary:bin_to_list(Decode#'AccountNewReq'.email),
    Password = Decode#'AccountNewReq'.password,
    {Msg, NewState} =
        case goblet_db:account_login(Email, Password) of
            true ->
                Record = goblet_db:account_by_email(Email),
                Players = Record#goblet_account.player_ids,
                Reply = goblet_pb:encode_msg(#'AccountLoginResp'{status = 'OK', players = Players}),
                {Reply, Email};
            false ->
                Reply = goblet_pb:encode_msg(#'AccountLoginResp'{
                    status = 'ERROR',
                    error = "invalid password"
                }),
                {Reply, false}
        end,
    OpCode = <<?ACCOUNT_LOGIN:16>>,
    {[OpCode, Msg], NewState}.

%%----------------------------------------------------------------------------
%% @doc Create a new player character
%% @end
%%----------------------------------------------------------------------------
-spec player_new(binary(), any()) -> {[binary(), ...], any()}.
% Let it crash when an unauthenticated user tries to make an account.
player_new(Message, State) when State#session.authenticated =:= true ->
    Decode = goblet_pb:decode_msg(Message, 'PlayerNewReq'),
    Name = binary:bin_to_list(Decode#'PlayerNewReq'.name),
    Title = binary:bin_to_list(Decode#'PlayerNewReq'.title),
    Appearance = Decode#'PlayerNewReq'.appearance,
    Role = binary:bin_to_list(Decode#'PlayerNewReq'.role),
    Account = State#session.email,
    Msg =
        case goblet_space_player:new(Name, Title, Appearance, Role, Account) of
            ok ->
                goblet_pb:encode_msg(#'PlayerNewResp'{status = 'OK'});
            {error, Error} ->
                goblet_pb:encode_msg(#'PlayerNewResp'{status = 'ERROR', error = Error})
        end,
    OpCode = <<?PLAYER_NEW:16>>,
    {[OpCode, Msg], State}.

%%============================================================================
%% Internal functions
%%============================================================================

sanitize_message(Message) ->
    %TODO: Check for message lengths, etc. Ensure that a client isn't DOSing
    %      other client(s)
    Message.
