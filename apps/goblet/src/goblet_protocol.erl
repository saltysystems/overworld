-module(goblet_protocol).

-export([
    decode/2,
    account_new/1,
    account_login/1,
    player_new/2,
    match_create/2,
    match_list/2,
    match_join/2,
    match_leave/2,
    match_info/2,
    match_prepare/2,
    match_decide/2
]).
-export([player_log/1]).

-include("goblet_opcode.hrl").
-include("goblet_database.hrl").
-include("goblet_pb.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(session, {email = none, authenticated = false, match = false}).

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
-spec decode(binary(), any()) -> {ok, any()} | {[binary(), ...], any()}.
decode(<<?VERSION:16, _T/binary>>, State) ->
    logger:notice("Version request~n"),
    {ok, State};
decode(<<?ACCOUNT_NEW:16, T/binary>>, _State) ->
    logger:notice("New account request~n"),
    account_new(T);
decode(<<?ACCOUNT_LOGIN:16, T/binary>>, _State) ->
    logger:notice("Account login request~n"),
    account_login(T);
decode(<<?PLAYER_NEW:16, T/binary>>, State) ->
    logger:notice("New player request~n~p", [State#session.email]),
    player_new(T, State);
decode(<<?PLAYER_LIST:16, T/binary>>, State) ->
    logger:notice("Player list request from ~p~n", [State#session.email]),
    player_new(T, State);
decode(<<?MATCH_LIST:16, T/binary>>, State) ->
    logger:notice("Match list request from ~p~n", [State#session.email]),
    match_list(T, State);
decode(<<?MATCH_CREATE:16, T/binary>>, State) ->
    logger:notice("New match request from ~p~n", [State#session.email]),
    match_create(T, State);
decode(<<?MATCH_JOIN:16, T/binary>>, State) ->
    logger:notice("Match join request from ~p~n", [State#session.email]),
    match_join(T, State);
decode(<<?MATCH_LEAVE:16, T/binary>>, State) ->
    logger:notice("Match leave requestfrom ~p~n", [State#session.email]),
    match_leave(T, State);
decode(<<?MATCH_START:16, T/binary>>, State) ->
    logger:notice("Match start request from ~p~n", [State#session.email]),
    match_start(T, State);
decode(<<?MATCH_STATE:16, T/binary>>, State) ->
    logger:notice("Match state request from ~p~n", [State#session.email]),
    match_info(T, State);
decode(<<?MATCH_PREPARE:16, T/binary>>, State) ->
    logger:notice("Match prepare packet from ~p~n", [State#session.email]),
    match_prepare(T, State);
decode(<<?MATCH_DECIDE:16, T/binary>>, State) ->
    logger:notice("Match decision packet from ~p~n", [State#session.email]),
    match_decide(T, State);
decode(<<OpCode:16, _T/binary>>, State) ->
    logger:notice("Got an unknown opcode ~p from ~p~n", [OpCode, State#session.email]),
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
                {Reply, #session{email = Email, authenticated = true}};
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
%% @doc Get the current lobby information
%% @end
%%----------------------------------------------------------------------------
-spec match_list(binary(), any()) -> {[binary(), ...], any()}.
match_list(_Message, State) ->
    Matches = goblet_lobby:get_matches(),
    % Convert the tuples back to records..
    M1 = [pack_match(X) || X <- Matches],
    Resp = #'ResponseObject'{status = 'OK'},
    Msg = goblet_pb:encode_msg(#'LobbyInfo'{resp = Resp, matches = M1}),
    OpCode = <<?MATCH_LIST:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Create a new match. Will only create matches for sessions where the
%%      player is authenticated.
%% @end
%%----------------------------------------------------------------------------
-spec match_create(binary(), any()) -> {[binary(), ...], any()}.
match_create(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchCreateReq'),
    Mode = Match#'MatchCreateReq'.mode,
    MaxPlayers = Match#'MatchCreateReq'.players_max,
    Extra =
        case Match#'MatchCreateReq'.extra of
            undefined -> <<>>;
            Bytes -> Bytes
        end,
    Msg =
        case goblet_lobby:create_match(Mode, MaxPlayers, Extra) of
            {ok, M} ->
                Resp = #'ResponseObject'{status = 'OK'},
                goblet_pb:encode_msg(#'MatchCreateResp'{resp = Resp, match = pack_match(M)});
            {error, Error} ->
                Resp = #'ResponseObject'{
                    status = 'ERROR',
                    error = atom_to_list(Error)
                },
                goblet_pb:encode_msg(#'MatchCreateResp'{resp = Resp})
        end,
    OpCode = <<?MATCH_CREATE:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Join a match. Will only join matches for sessions where the player is
%%      authenticated. Additionally registers the connection in the process
%%      registry for the match.
%% @end
%%----------------------------------------------------------------------------
-spec match_join(binary(), any()) -> {[binary(), ...], any()}.
match_join(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchJoinReq'),
    MatchID = Match#'MatchJoinReq'.matchid,
    Player = binary:bin_to_list(Match#'MatchJoinReq'.player),
    IsValid = goblet_db:is_valid_player_account(Player, State#session.email),
    match_join(MatchID, Player, State, IsValid).

%%----------------------------------------------------------------------------
%% @doc (unexported) Only actually commit joining a match once it has been
%%      validated that the proposed player actually belongs to the account for
%%      whom the session is validated.
%% @end
%%----------------------------------------------------------------------------
match_join(MatchID, Player, State, true) ->
    Msg =
        case goblet_lobby:join_match(Player, MatchID) of
            {ok, M} ->
                % Register the current process with process registry
                match_register_session(MatchID),
                Resp = #'ResponseObject'{status = 'OK'},
                goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp, match = pack_match(M)});
            {error, Error} ->
                Resp = #'ResponseObject'{
                    status = 'ERROR',
                    error = atom_to_list(Error)
                },
                goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp})
        end,
    OpCode = <<?MATCH_JOIN:16>>,
    {[OpCode, Msg], State};
match_join(_MatchID, _Player, State, false) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = "player_account_mismatch"
    },
    Msg = goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp}),
    OpCode = <<?MATCH_JOIN:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Leave a match. Will only leave matches for sessions where the player
%%      is authenticated.
%% @end
%%----------------------------------------------------------------------------
-spec match_leave(binary(), any()) -> {[binary(), ...], any()}.
match_leave(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchLeaveReq'),
    MatchID = Match#'MatchJoinReq'.matchid,
    Player = Match#'MatchJoinReq'.player,
    Msg =
        case goblet_lobby:leave_match(Player, MatchID) of
            ok ->
                Resp = #'ResponseObject'{status = 'OK'},
                match_deregister_session(MatchID),
                goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp});
            {error, Error} ->
                Resp = #'ResponseObject'{
                    status = 'ERROR',
                    error = atom_to_list(Error)
                },
                goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp})
        end,
    OpCode = <<?MATCH_LEAVE:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Start a match. The first player the match (i.e., the head of the
%%      player list) controls when to start the match.
%% @end
%%----------------------------------------------------------------------------
-spec match_start(binary(), any()) -> {[binary(), ...], any()}.
match_start(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchStartReq'),
    MatchID = Match#'MatchStartReq'.matchid,
    Player = binary:bin_to_list(Match#'MatchStartReq'.player),
    Email = State#session.email,
    IsValid =
        case
            goblet_util:run_checks([
                fun() -> check_valid_player_account(Player, Email) end,
                fun() -> check_valid_match_owner(Player, MatchID) end
            ])
        of
            ok -> true;
            Error -> Error
        end,
    match_start(MatchID, State, IsValid).

match_start(MatchID, State, true) ->
    Msg =
        case goblet_lobby:start_match(MatchID) of
            ok ->
                Resp = #'ResponseObject'{status = 'OK'},
                goblet_pb:encode_msg(#'MatchStartResp'{resp = Resp});
            {error, Error} ->
                Resp = #'ResponseObject'{
                    status = 'ERROR',
                    error = atom_to_list(Error)
                },
                goblet_pb:encode_msg(#'MatchStartResp'{resp = Resp})
        end,
    OpCode = <<?MATCH_START:16>>,
    match_broadcast([OpCode, Msg], MatchID),
    {ok, State};
match_start(_MatchID, State, {error, ErrMsg}) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = atom_to_list(ErrMsg)
    },
    Msg = goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp}),
    OpCode = <<?MATCH_START:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Return the current match info
%% @end
%%----------------------------------------------------------------------------
-spec match_info(binary(), any()) -> {[binary(), ...], any()}.
match_info(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchStateReq'),
    MatchID = Match#'MatchInfoReq'.matchid,
    Player = binary:bin_to_list(Match#'MatchInfoReq'.player),
    Email = State#session.email,
    IsValid =
        case
            goblet_util:run_checks([
                fun() -> check_valid_player_account(Player, Email) end,
                fun() -> check_valid_match_player(Player, Email) end
            ])
        of
            ok -> true;
            Error -> Error
        end,
    Msg = match_info(MatchID, State, IsValid),
    OpCode = <<?MATCH_INFO:16>>,
    {[OpCode, Msg], State}.

match_info(MatchID, State, true) ->
    Match = pack_match(goblet_lobby:get_match(MatchID)),
    Msg = goblet_pb:encode_msg(Match);
match_info(_MatchID, State, {error, ErrMsg}) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = atom_to_list(ErrMsg)
    },
    Msg = goblet_pb:encode_msg(#'MatchInfoResp'{resp = Resp}).

%%----------------------------------------------------------------------------
%% @doc Accept parameters for a player's preparation phase
%% @end
%%----------------------------------------------------------------------------
-spec match_prepare(binary(), any()) -> {ok | [binary(), ...], any()}.
match_prepare(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchPrepReq'),
    MatchID = Match#'MatchPrepReq'.matchid,
    Player = binary:bin_to_list(Match#'MatchPrepReq'.player),
    Email = State#session.email,
    IsValid =
        case
            goblet_util:run_checks([
                fun() -> check_valid_player_account(Player, Email) end,
                fun() -> check_valid_match_player(Player, Email) end
            ])
        of
            ok -> true;
            Error -> Error
        end,
    match_prepare(MatchID, Player, State, IsValid).

match_prepare(MatchID, Player, State, true) ->
    goblet_instance:player_ready(Player, MatchID),
    %TODO: broadcast message of the internal match state
    %TODO x2: come up with a new name for the external state vs the internal
    %state
    {ok, State};
match_prepare(_MatchID, _Player, State, {error, ErrMsg}) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = atom_to_list(ErrMsg)
    },
    Msg = goblet_pb:encode_msg(#'MatchPrepResp'{resp = Resp}),
    OpCode = <<?MATCH_PREPARE:16>>,
    [{OpCode, Msg}, State].

%%----------------------------------------------------------------------------
%% @doc Accept a player's decisions and forward them to the state machine
%% @end
%%----------------------------------------------------------------------------
-spec match_decide(binary(), any()) -> {ok | [binary(), ...], any()}.
match_decide(Message, State) when State#session.authenticated =:= true ->
    Match = goblet_pb:decode_msg(Message, 'MatchDecideReq'),
    MatchID = Match#'MatchDecideReq'.matchid,
    Player = binary:bin_to_list(Match#'MatchDecideReq'.player),
    %what and how will they validate?
    Actions = Match#'MatchDecideReq'.actions,
    Email = State#session.email,
    IsValid =
        case
            goblet_util:run_checks([
                fun() -> check_valid_player_account(Player, Email) end,
                fun() -> check_valid_match_player(Player, Email) end
                % TODO: fun() -> check_valid_actions ?
            ])
        of
            ok -> true;
            Error -> Error
        end,
    match_decide(MatchID, Player, Actions, State, IsValid).

match_decide(MatchID, Player, Actions, State, true) ->
    goblet_instance:player_decision(Player, Actions, MatchID),
    {ok, State};
match_decide(_MatchID, _Player, _Actions, State, {error, ErrMsg}) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = atom_to_list(ErrMsg)
    },
    Msg = goblet_pb:encode_msg(#'MatchPrepResp'{resp = Resp}),
    OpCode = <<?MATCH_PREPARE:16>>,
    [{OpCode, Msg}, State].

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

match_register_session(MatchID) ->
    gproc:reg({p, l, {match, MatchID}}).

-spec match_broadcast([binary(), ...], integer()) -> ok.
match_broadcast(Message, MatchID) ->
    gproc:send({p, l, {match, MatchID}}, {self(), event, Message}).

match_deregister_session(MatchID) ->
    gproc:unreg({p, l, {match, MatchID}}).

% TODO: fix me up, not quite what we want. we need internal representation
%match_broadcast_state(MatchID) ->
%    Match = pack_match(goblet_lobby:get_match(MatchID)),
%    Msg = goblet_pb:encode_msg(Match),
%    match_broadcast(Msg, MatchID).

pack_match({Id, State, Players, PlayersMax, StartTime, Mode, Extra}) ->
    #'Match'{
        id = Id,
        state = State,
        players = Players,
        players_max = PlayersMax,
        start_time = StartTime,
        duration = (erlang:system_time(second) - StartTime),
        mode = Mode,
        extra = Extra
    }.

check_valid_player_account(Player, Email) ->
    case goblet_db:is_valid_player_account(Player, Email) of
        true -> ok;
        false -> {error, invalid_account}
    end.

check_valid_match_owner(Player, MatchID) ->
    case goblet_lobby:get_match_players(MatchID) of
        [H | T] ->
            case H =:= Player of
                true -> ok;
                false -> {error, not_match_owner}
            end;
        {error, E} ->
            {error, E}
    end.

check_valid_match_player(Player, MatchID) ->
    Players = goblet_lobby:get_match_players(MatchID),
    case lists:member(Player, Players) of
        true -> ok;
        false -> {error, not_in_match}
    end.

%%============================================================================
%% Tests
%%============================================================================

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
    Msg = goblet_pb:encode_msg(#'AccountNewReq'{email = Email, password = Password}),
    {[RespOp, RespMsg], _State} = goblet_protocol:account_new(Msg),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountNewResp'),
    ?assertEqual('OK', DecodedResp#'AccountNewResp'.status).

account_new_already_exists_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = goblet_pb:encode_msg(#'AccountNewReq'{email = Email, password = Password}),
    {[RespOp, RespMsg], _State} = goblet_protocol:account_new(Msg),
    ?assertEqual(<<?ACCOUNT_NEW:16>>, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountNewResp'),
    ?assertEqual('ERROR', DecodedResp#'AccountNewResp'.status),
    ?assertEqual(<<"email_already_registered">>, DecodedResp#'AccountNewResp'.error).

account_login_bad_password_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "Password",
    Msg = goblet_pb:encode_msg(#'AccountLoginReq'{email = Email, password = Password}),
    {[RespOp, RespMsg], _State} = goblet_protocol:account_login(Msg),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountLoginResp'),
    ?assertEqual('ERROR', DecodedResp#'AccountLoginResp'.status).

account_login_test() ->
    Email = "TestUser@doesntexist.notadomain",
    Password = "TestPassword1234",
    Msg = goblet_pb:encode_msg(#'AccountLoginReq'{email = Email, password = Password}),
    {[RespOp, RespMsg], _State} = goblet_protocol:account_login(Msg),
    ?assertEqual(<<?ACCOUNT_LOGIN:16>>, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'AccountLoginResp'),
    ?assertEqual('OK', DecodedResp#'AccountLoginResp'.status),
    ?assertEqual([], DecodedResp#'AccountLoginResp'.players).

player_new_test() ->
    % mock the state
    Email = "TestUser@doesntexist.notadomain",
    State = #session{email = Email, authenticated = true},
    Message = goblet_pb:encode_msg(#'PlayerNewReq'{
        name = "Chester The Tester",
        title = "Rickety Mockup",
        appearance = 1,
        role = "interceptor"
    }),
    {[RespOp, RespMsg], _State} = goblet_protocol:player_new(Message, State),
    ?assertEqual(RespOp, <<?PLAYER_NEW:16>>),
    RespObj = goblet_pb:decode_msg(RespMsg, 'PlayerNewResp'),
    ?assertEqual(RespObj#'PlayerNewResp'.status, 'OK').

match_list_test() ->
    % shouldnt matter here
    State = #session{},
    {[RespOp, RespMsg], _State} = goblet_protocol:match_list(<<>>, State),
    ?assertEqual(<<?MATCH_LIST:16>>, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'LobbyInfo'),
    ResponseObj = DecodedResp#'LobbyInfo'.resp,
    ?assertEqual(ResponseObj#'ResponseObject'.status, 'OK'),
    ?assertEqual(is_list(DecodedResp#'LobbyInfo'.matches), true).

match_create_test() ->
    Email = "TestUser@doesntexist.notadomain",
    State = #session{email = Email, authenticated = true},
    Mode = 'DEFAULT',
    MaxPlayers = 6,
    Msg = goblet_pb:encode_msg(#'MatchCreateReq'{mode = Mode, players_max = MaxPlayers}),
    {[RespOp, RespMsg], State} = goblet_protocol:match_create(Msg, State),
    OpCode = <<?MATCH_CREATE:16>>,
    ?assertEqual(OpCode, RespOp),
    DecodedResp = goblet_pb:decode_msg(RespMsg, 'MatchCreateResp'),
    ResponseObj = DecodedResp#'MatchCreateResp'.resp,
    ?assertEqual(ResponseObj#'ResponseObject'.status, 'OK'),
    M = DecodedResp#'MatchCreateResp'.match,
    ?assertEqual(MaxPlayers, M#'Match'.players_max),
    ?assertEqual(Mode, M#'Match'.mode),
    % deconstruct the match
    goblet_lobby:delete_match(M#'Match'.id).
