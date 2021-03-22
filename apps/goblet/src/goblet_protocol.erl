-module(goblet_protocol).

-export([
    decode/2,
    account_new/1,
    account_login/1,
    player_new/2,
    match_create/2,
    match_list/2,
    match_join/2,
    match_leave/2
]).
-export([player_log/1]).

-include("goblet_opcode.hrl").
-include("goblet_database.hrl").
-include("goblet_pb.hrl").

-include_lib("kernel/include/logger.hrl").

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
    logger:notice("Match start request from ~p~n", [State#session.email]);
decode(<<?MATCH_STATE:16, T/binary>>, State) ->
    logger:notice("Match state request from ~p~n", [State#session.email]);
decode(<<?MATCH_PREPARE:16, T/binary>>, State) ->
    logger:notice("Match prepare packet from ~p~n", [State#session.email]);
decode(<<?MATCH_DECIDE:16, T/binary>>, State) ->
    logger:notice("Match decision packet from ~p~n", [State#session.email]);
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
%%      authenticated.
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
%% @doc Only actually commit joining a match once it has been validated that
%%      the proposed player actually belongs to the account for whom the
%%      session is validated.
%% @end
%%----------------------------------------------------------------------------
match_join(MatchID, Player, State, true) ->
    Msg =
        case goblet_lobby:join_match(Player, MatchID) of
            {ok, M} ->
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
match_join(MatchID, Player, State, false) ->
    Resp = #'ResponseObject'{
        status = 'ERROR',
        error = "player_account_mismatch"
    },
    Msg = goblet_pb:encode_msg(#'MatchJoinResp'{resp = Resp}),
    OpCode = <<?MATCH_JOIN:16>>,
    {[OpCode, Msg], State}.

%%----------------------------------------------------------------------------
%% @doc Leave a match. Will only leave matches for sessions where the player is
%%      authenticated.
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
