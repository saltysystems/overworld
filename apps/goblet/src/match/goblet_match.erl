-module(goblet_match).

%-export([
%    create/2,
%    list/2,
%    join/2,
%    leave/2,
%    start/2,
%    info/2,
%    prepare/2,
%    decide/2,
%    state_update/7,
%    broadcast/2,
%    broadcast_intent/4
%]).
%
%-include("net/goblet_session.hrl").
%-include("db/goblet_database.hrl").
%-include("shipgame_pb.hrl").
%
%-include_lib("kernel/include/logger.hrl").
%-include_lib("eunit/include/eunit.hrl").
%
%-record(session, {email, authenticated, game_info}).
%
%%
%%%% Lobby and match creation commands
%-define(PLAYER_NEW, 16#1050).
%-define(MATCH_LIST, 16#2010).
%-define(MATCH_CREATE, 16#2020).
%-define(MATCH_JOIN, 16#2030).
%-define(MATCH_LEAVE, 16#2040).
%-define(MATCH_START, 16#2050).
%-define(MATCH_INFO, 16#2060).
%-define(MATCH_STATE, 16#2070).
%-define(MATCH_PREPARE, 16#2080). % acknowledge match ready
%-define(MATCH_DECIDE, 16#2082).  % send decision
%-define(MATCH_EXECUTE, 16#2084). % cue to play animations
%-define(MATCH_FINISH,16#2086). % show summary screen
%-define(MATCH_INTENT, 16#2088). % send notification of an intended action
%
%
%%%----------------------------------------------------------------------------
%%% @doc Get the current lobby information
%%% @end
%%%----------------------------------------------------------------------------
%-spec list(binary(), tuple()) -> {[binary(), ...], tuple()}.
%list(_Message, State) ->
%    Matches = goblet_lobby:get_matches(),
%    % Convert the tuples back to records..
%    M1 = [pack_match(X) || X <- Matches],
%    Resp = #'ResponseObject'{status = 'OK'},
%    Msg = shipgame_pb:encode_msg(#'MatchListResp'{resp = Resp, matches = M1}),
%    OpCode = <<?MATCH_LIST:16>>,
%    {[OpCode, Msg], State}.
%
%list_test() ->
%    % shouldnt matter here
%    State = #session{},
%    {[RespOp, RespMsg], _State} = list(<<>>, State),
%    ?assertEqual(<<?MATCH_LIST:16>>, RespOp),
%    DecodedResp = shipgame_pb:decode_msg(RespMsg, 'MatchListResp'),
%    ResponseObj = DecodedResp#'MatchListResp'.resp,
%    ?assertEqual(ResponseObj#'ResponseObject'.status, 'OK'),
%    ?assertEqual(is_list(DecodedResp#'MatchListResp'.matches), true).
%
%
%%%----------------------------------------------------------------------------
%%% @doc Create a new match. Will only create matches for sessions where the
%%%      player is authenticated.
%%% @end
%%%----------------------------------------------------------------------------
%-spec create(binary(), tuple()) -> {[binary(), ...], tuple()}.
%create(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchCreateReq'),
%    Player = Match#'MatchCreateReq'.player,
%    Mode = Match#'MatchCreateReq'.mode,
%    MaxPlayers = Match#'MatchCreateReq'.players_max,
%    Email = State#session.email,
%    Extra =
%        case Match#'MatchCreateReq'.extra of
%            undefined -> <<>>;
%            Bytes -> Bytes
%        end,
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end
%            ])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    create(Player, Mode, MaxPlayers, Extra, State, IsValid).
%
%
%create(Player, Mode, MaxPlayers, Extra, State0, true) ->
%    {Msg, State1} =
%        case goblet_lobby:create_match(Player, Mode, MaxPlayers, Extra) of
%            {ok, MatchID} ->
%                {ok, M} = goblet_lobby:get_match(MatchID),
%                register_session(MatchID),
%                Resp = #'ResponseObject'{status = 'OK'},
%                {
%                    shipgame_pb:encode_msg(#'MatchCreateResp'{
%                        resp = Resp,
%                        match = pack_match(M)
%                    }),
%                    State0#session{game_info = {MatchID,Player}}
%                };
%            {error, Error} ->
%                Resp = #'ResponseObject'{
%                    status = 'ERROR',
%                    error = atom_to_list(Error)
%                },
%                {
%                    shipgame_pb:encode_msg(#'MatchCreateResp'{resp = Resp}),
%                    State0
%                }
%        end,
%    OpCode = <<?MATCH_CREATE:16>>,
%    {[OpCode, Msg], State1};
%create(_Player, _Mode, _MaxPlayers, _Extra, State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    Msg = shipgame_pb:encode_msg(#'MatchCreateResp'{resp = Resp}),
%    OpCode = <<?MATCH_CREATE:16>>,
%    {[OpCode, Msg], State}.
%
%
%create_leave_test() ->
%    Email = "TestUser@doesntexist.notadomain",
%    State = #session{email = Email, authenticated = true},
%    Mode = 'DEFAULT',
%    MaxPlayers = 6,
%    Player = "Chester The Tester",
%    Msg = shipgame_pb:encode_msg(#'MatchCreateReq'{
%        player = Player,
%        mode = Mode,
%        players_max = MaxPlayers
%    }),
%    {[RespOp, RespMsg], State} = create(Msg, State),
%    OpCode = <<?MATCH_CREATE:16>>,
%    ?assertEqual(OpCode, RespOp),
%    DecodedResp = shipgame_pb:decode_msg(RespMsg, 'MatchCreateResp'),
%    ResponseObj = DecodedResp#'MatchCreateResp'.resp,
%    ?assertEqual(ResponseObj#'ResponseObject'.status, 'OK'),
%    M = DecodedResp#'MatchCreateResp'.match,
%    ?assertEqual(MaxPlayers, M#'MatchInfo'.players_max),
%    ?assertEqual(Mode, M#'MatchInfo'.mode),
%    % Now try to leave
%    Msg1 = shipgame_pb:encode_msg(#'MatchLeaveReq'{
%        player = Player,
%        matchid = M#'MatchInfo'.id
%    }),
%    {[RespOp1, RespMsg1], State} = leave(Msg1, State),
%    ?assertEqual(<<?MATCH_LEAVE:16>>, RespOp1),
%    DecodedResp1 = shipgame_pb:decode_msg(RespMsg1, 'MatchLeaveResp'),
%    Response1 = DecodedResp1#'MatchLeaveResp'.resp,
%    ?assertEqual(Response1#'ResponseObject'.status, 'OK').
%
%
%%%-------------------------------------------------------------------------
%%% @doc Join a match. Will only join matches for sessions where the player
%%%      is authenticated. Additionally registers the connection in the
%%%      process registry for the match.
%%% @end
%%%-------------------------------------------------------------------------
%-spec join(binary(), tuple()) -> {[binary(), ...], tuple()}.
%join(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchJoinReq'),
%    MatchID = Match#'MatchJoinReq'.matchid,
%    Player = Match#'MatchJoinReq'.player,
%    Email = State#session.email,
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end
%            ])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    join(MatchID, Player, State, IsValid).
%
%join(MatchID, Player, State, true) ->
%    Msg =
%        case goblet_lobby:join_match(Player, MatchID) of
%            {ok, M} ->
%                % Register the current process with process registry
%                register_session(MatchID),
%                Resp = #'ResponseObject'{status = 'OK'},
%                shipgame_pb:encode_msg(#'MatchJoinResp'{
%                    resp = Resp,
%                    match = pack_match(M)
%                });
%            {error, Error} ->
%                Resp = #'ResponseObject'{
%                    status = 'ERROR',
%                    error = atom_to_list(Error)
%                },
%                shipgame_pb:encode_msg(#'MatchJoinResp'{resp = Resp})
%        end,
%    OpCode = <<?MATCH_JOIN:16>>,
%    {[OpCode, Msg], State#session{game_info={MatchID, Player}}};
%join(_MatchID, _Player, State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    Msg = shipgame_pb:encode_msg(#'MatchJoinResp'{resp = Resp}),
%    OpCode = <<?MATCH_JOIN:16>>,
%    {[OpCode, Msg], State}.
%
%join_test() ->
%    logger:notice("Gproc info: ~p", [gproc:info(self())]),
%    % Create the match
%    Email = "TestUser@doesntexist.notadomain",
%    State = #session{email = Email, authenticated = true},
%    Mode = 'DEFAULT',
%    MaxPlayers = 6,
%    Msg = shipgame_pb:encode_msg(#'MatchCreateReq'{
%        player = "Chester The Tester",
%        mode = Mode,
%        players_max = MaxPlayers
%    }),
%    % Note that creating _also_ joins, which tripped me up when debugging
%    % gproc errors
%    {[RespOp, RespMsg], State} = create(Msg, State),
%    ?assertEqual(RespOp, <<?MATCH_CREATE:16>>),
%    DecodedResp = shipgame_pb:decode_msg(RespMsg, 'MatchCreateResp'),
%    M = DecodedResp#'MatchCreateResp'.match,
%    MatchID = M#'MatchInfo'.id,
%    % Deregister the current process from gproc
%    deregister_session(MatchID),
%    % Create a new player and join the match
%    Player2 = "Lester The Tester",
%    Message2 = shipgame_pb:encode_msg(#'PlayerNewReq'{
%        name = Player2,
%        color = ["#000000", "#ffffff", "#c0ffee"],
%        symbol = [1, 3],
%        role = 'DESTROYER'
%    }),
%    {[RespOp2, _RespMsg2], _State} = goblet_player:new(
%        Message2,
%        State
%    ),
%    ?assertEqual(RespOp2, <<?PLAYER_NEW:16>>),
%    % Create another new player and join the match
%    Player3 = "Nester The Tester",
%    Message3 = shipgame_pb:encode_msg(#'PlayerNewReq'{
%        name = Player3,
%        color = ["#000000", "#ffffff", "#c0ffee"],
%        symbol = [1, 3],
%        role = 'DESTROYER'
%    }),
%    {[RespOp3, _RespMsg3], _} = goblet_player:new(
%        Message3,
%        State
%    ),
%    ?assertEqual(RespOp3, <<?PLAYER_NEW:16>>),
%
%    JoinMsg2 = shipgame_pb:encode_msg(#'MatchJoinReq'{
%        player = Player2,
%        matchid = MatchID
%    }),
%
%    JoinMsg3 = shipgame_pb:encode_msg(#'MatchJoinReq'{
%        player = Player3,
%        matchid = MatchID
%    }),
%
%    {[RespOp4, RespMsg4], _} = join(
%        JoinMsg2,
%        State
%    ),
%    ?assertEqual(RespOp4, <<?MATCH_JOIN:16>>),
%    Msg4 = shipgame_pb:decode_msg(RespMsg4, 'MatchJoinResp'),
%    RespObj = Msg4#'MatchJoinResp'.resp,
%    ?assertEqual('OK', RespObj#'ResponseObject'.status),
%    deregister_session(MatchID),
%
%    {[RespOp5, RespMsg5], _} = join(
%        JoinMsg3,
%        State
%    ),
%    deregister_session(MatchID),
%    ?assertEqual(RespOp5, <<?MATCH_JOIN:16>>),
%    Msg5 = shipgame_pb:decode_msg(RespMsg5, 'MatchJoinResp'),
%    RespObj = Msg5#'MatchJoinResp'.resp,
%    ?assertEqual('OK', RespObj#'ResponseObject'.status).
%
%
%%%-------------------------------------------------------------------------
%%% @doc Leave a match. Will only leave matches for sessions where the player
%%%      is authenticated.
%%% @end
%%%-------------------------------------------------------------------------
%-spec leave(binary(), tuple()) -> {[binary(), ...], tuple()}.
%leave(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchLeaveReq'),
%    MatchID = Match#'MatchLeaveReq'.matchid,
%    Player = Match#'MatchLeaveReq'.player,
%    Msg =
%        case goblet_lobby:leave_match(Player, MatchID) of
%            ok ->
%                Resp = #'ResponseObject'{status = 'OK'},
%                deregister_session(MatchID),
%                shipgame_pb:encode_msg(#'MatchLeaveResp'{resp = Resp});
%            {error, Error} ->
%                Resp = #'ResponseObject'{
%                    status = 'ERROR',
%                    error = atom_to_list(Error)
%                },
%                shipgame_pb:encode_msg(#'MatchLeaveResp'{resp = Resp})
%        end,
%    OpCode = <<?MATCH_LEAVE:16>>,
%    {[OpCode, Msg], State}.
%
%
%
%%%-------------------------------------------------------------------------
%%% @doc Start a match. The first player the match (i.e., the head of the
%%%      player list) controls when to start the match.
%%% @end
%%%-------------------------------------------------------------------------
%-spec start(binary(), tuple()) -> {[binary(), ...], tuple()}.
%start(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchStartReq'),
%    MatchID = Match#'MatchStartReq'.matchid,
%    Player = Match#'MatchStartReq'.player,
%    Email = State#session.email,
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end,
%                fun() -> check_valid_match_owner(Player, MatchID) end
%            ])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    start(MatchID, State, IsValid).
%
%start(MatchID, State, true) ->
%    Msg =
%        case goblet_lobby:start_match(MatchID) of
%            ok ->
%                Resp = #'ResponseObject'{status = 'OK'},
%                shipgame_pb:encode_msg(#'MatchStartResp'{resp = Resp});
%            {error, Error} ->
%                Resp = #'ResponseObject'{
%                    status = 'ERROR',
%                    error = atom_to_list(Error)
%                },
%                shipgame_pb:encode_msg(#'MatchStartResp'{resp = Resp})
%        end,
%    OpCode = <<?MATCH_START:16>>,
%    broadcast([OpCode, Msg], MatchID),
%    {ok, State};
%start(_MatchID, State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    Msg = shipgame_pb:encode_msg(#'MatchJoinResp'{resp = Resp}),
%    OpCode = <<?MATCH_START:16>>,
%    {[Msg, OpCode], State}.
%
%
%
%%%-------------------------------------------------------------------------
%%% @doc Return the current match info
%%% @end
%%%-------------------------------------------------------------------------
%%% TODO: Understand the badarg around the IsValid check
%-spec info(binary(), tuple()) -> {[binary(), ...], tuple()}.
%info(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchInfoReq'),
%    MatchID = Match#'MatchInfoReq'.matchid,
%    Player = Match#'MatchInfoReq'.player,
%    Email = State#session.email,
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end,
%                fun() -> check_valid_match_owner(Player, MatchID) end
%            ])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    Msg = info(MatchID, State, IsValid),
%    OpCode = <<?MATCH_INFO:16>>,
%    {[OpCode, Msg], State}.
%
%info(MatchID, _State, true) ->
%    case goblet_lobby:get_match(MatchID) of
%        {ok, M} ->
%            Resp = #'ResponseObject'{status = 'OK'},
%            shipgame_pb:encode_msg(#'MatchInfoResp'{
%                resp = Resp,
%                match = pack_match(M)
%            });
%        {error, Error} ->
%            Resp = #'ResponseObject'{
%                status = 'ERROR',
%                error = atom_to_list(Error)
%            },
%            shipgame_pb:encode_msg(#'MatchInfoResp'{resp = Resp})
%    end;
%info(_MatchID, _State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    shipgame_pb:encode_msg(#'MatchInfoResp'{resp = Resp}).
%
%%%-------------------------------------------------------------------------
%%% @doc Accept parameters for a player's preparation phase
%%% @end
%%%-------------------------------------------------------------------------
%-spec prepare(binary(), tuple()) -> {ok | [binary(), ...], tuple()}.
%prepare(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchPrepReq'),
%    MatchID = Match#'MatchPrepReq'.matchid,
%    Player = Match#'MatchPrepReq'.player,
%    Email = State#session.email,
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end
%            ])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    prepare(MatchID, Player, State, IsValid).
%
%prepare(MatchID, Player, State, true) ->
%    goblet_instance:player_ready(Player, MatchID),
%    {ok, State};
%prepare(_MatchID, _Player, State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    Msg = shipgame_pb:encode_msg(#'MatchPrepResp'{resp = Resp}),
%    OpCode = <<?MATCH_PREPARE:16>>,
%    [{OpCode, Msg}, State].
%
%
%
%%%-------------------------------------------------------------------------
%%% @doc Accept a player's decisions and forward them to the state machine
%%% @end
%%%-------------------------------------------------------------------------
%-spec decide(binary(), tuple()) -> {ok | [binary(), ...], tuple()}.
%decide(Message, State) when State#session.authenticated =:= true ->
%    Match = shipgame_pb:decode_msg(Message, 'MatchDecideReq'),
%    MatchID = Match#'MatchDecideReq'.matchid,
%    Player = Match#'MatchDecideReq'.player,
%    Actions = action_to_tuple(Player, Match#'MatchDecideReq'.actions),
%    Inv = goblet_db:player_inventory(Player),
%    Email = State#session.email,
%    % Let's lay out all of the things that need to be true for an action
%    % to go through.
%    %   1. Must be a valid player account
%    %   2. Player must be in the match
%    %   3. Player must be alive currently
%    %   4. Player must have an item with that action
%    %   5. Action must have a valid target
%    %   6. The action must not occur out of bounds
%    %   TODO: Validate that the target is the correct type of target (coords
%    %   vs object name). If there's no coordinates and no target, must throw
%    %   an error.
%    IsValid =
%        case
%            goblet_util:run_checks([
%                fun() -> check_valid_player_account(Player, Email) end,
%                %fun() -> check_valid_match_player(Player, Email) end, %BROKEN
%                fun() -> goblet_game:check_player_alive(Player) end,
%                fun() -> goblet_game:check_valid_items(Actions, Inv) end,
%                fun() ->
%                    goblet_game:check_valid_target(Actions, MatchID)
%                end])
%        of
%            ok -> true;
%            Error -> Error
%        end,
%    decide(MatchID, Player, Actions, State, IsValid).
%
%decide(MatchID, Player, Actions, State, true) ->
%    goblet_instance:player_decision(Player, Actions, MatchID),
%    {ok, State};
%decide(_MatchID, _Player, _Actions, State, {error, ErrMsg}) ->
%    Resp = #'ResponseObject'{
%        status = 'ERROR',
%        error = atom_to_list(ErrMsg)
%    },
%    Msg = shipgame_pb:encode_msg(#'MatchPrepResp'{resp = Resp}),
%    OpCode = <<?MATCH_PREPARE:16>>,
%    [{OpCode, Msg}, State].
%
%%%------------------------------------------------------------------------
%%% @doc Update a match state
%%% @end
%%%------------------------------------------------------------------------
%-spec state_update(
%    list(),
%    list(),
%    atom(),
%    list(),
%    list(),
%    integer(),
%    pos_integer()
%) -> ok.
%state_update(
%    Board,
%    Replay,
%    MatchState,
%    PlayerList,
%    ReadyPlayers,
%    Timer,
%    MatchID
%) ->
%    % TODO: Decide if we want to have flags or not
%    B1 = [
%        #'MatchStateResp.Tile'{x = X0, y = Y0, type = T0, occupant = W0}
%        || {X0, Y0, T0, W0} <- Board
%    ],
%    R1 = [
%        #'MatchStateResp.Action'{type = T1, who = W1, x = X1, y = Y1}
%        || {W1, T1, {X1, Y1}} <- Replay
%    ],
%    % If the timer is greater than zero, the client should know. Otherwise we can just throw it out.
%    Update =
%        case Timer > 0 of
%            true ->
%                #'MatchStateResp'{
%                    state = MatchState,
%                    board = B1,
%                    playerlist = PlayerList,
%                    readyplayers = ReadyPlayers,
%                    replay = R1,
%                    timer = Timer
%                };
%            false ->
%                #'MatchStateResp'{
%                    state = MatchState,
%                    board = B1,
%                    playerlist = PlayerList,
%                    readyplayers = ReadyPlayers,
%                    replay = R1
%                }
%        end,
%    Msg = shipgame_pb:encode_msg(Update),
%    OpCode = <<?MATCH_STATE:16>>,
%    broadcast([OpCode, Msg], MatchID).
%
%
%state_update_test() ->
%    B0 = [
%        #'MatchStateResp.Tile'{
%            x = 1,
%            y = 2,
%            type = "f",
%            occupant = ["Chester"],
%            flags = []
%        }
%    ],
%    R0 = [
%        #'MatchStateResp.Action'{
%            type = "move",
%            who = "Chester",
%            x = 1,
%            y = 3
%        }
%    ],
%    M = #'MatchStateResp'{
%        state = 'EXECUTE',
%        board = B0,
%        playerlist = ["Chester"],
%        readyplayers = ["Chester"],
%        replay = R0
%    },
%    Bin = shipgame_pb:encode_msg(M),
%    % succesful encoding
%    ?assertEqual(true, is_binary(Bin)).
%
%%%------------------------------------------------------------------------
%%% @doc Broadcast the intented action from a player
%%% @end
%%%------------------------------------------------------------------------
%-spec broadcast_intent(
%    list(),
%    list(),
%    {pos_integer(), pos_integer()},
%    pos_integer()
%) -> ok.
%broadcast_intent(Who, Type, {X, Y}, MatchID) ->
%    Action = #'MatchIntentResp.Action'{
%        type = Type,
%        x = X,
%        y = Y
%    },
%    M = #'MatchIntentResp'{player = Who, action = Action},
%    Msg = shipgame_pb:encode_msg(M),
%    OpCode = <<?MATCH_INTENT:16>>,
%    broadcast([OpCode, Msg], MatchID).
%
%broadcast_intent_encode_test() ->
%    % Test the protobuf messages
%    Who = "Chester",
%    Type = "move",
%    X = 4,
%    Y = 3,
%    Action = #'MatchIntentResp.Action'{
%        type = Type,
%        x = X,
%        y = Y
%    },
%    M = #'MatchIntentResp'{player = Who, action = Action},
%    Msg = shipgame_pb:encode_msg(M),
%    ?assertEqual(true, is_binary(Msg)).
%
%
%-spec broadcast([binary(), ...], integer()) -> ok.
%broadcast(Message, MatchID) ->
%    logger:debug("Broadcasting a message to match ~p", [MatchID]),
%    gproc:send({p, l, {match, MatchID}}, {self(), event, Message}).
%
%
%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%
%check_valid_match_owner(Player, MatchID) ->
%    case goblet_lobby:get_match_players(MatchID) of
%        [H | _T] ->
%            case H =:= Player of
%                true -> ok;
%                false -> {error, not_match_owner}
%            end;
%        {error, E} ->
%            {error, E}
%    end.
%
%check_valid_player_account(Player, Email) ->
%    case goblet_db:is_valid_player_account(Player, Email) of
%        true -> ok;
%        false -> {error, invalid_account}
%    end.
%
%% TODO: Fix this function. It keeps crashing somehow.
%%check_valid_match_player(Player, MatchID) ->
%%    Players = goblet_lobby:get_match_players(MatchID),
%%    case lists:member(Player, Players) of
%%        true -> ok;
%%        false -> {error, not_in_match}
%%    end.
%
%
%action_to_tuple(Player, L) ->
%    action_to_tuple(Player, L, []).
%
%action_to_tuple(_Player, [], Acc) ->
%    Acc;
%action_to_tuple(Player, [H | T], Acc) ->
%    Item = H#'MatchDecideReq.Action'.item,
%    Coordinates =
%        {H#'MatchDecideReq.Action'.x, H#'MatchDecideReq.Action'.y},
%    Target = H#'MatchDecideReq.Action'.target,
%    % First check if the target type is "Target", for tracking weapons.
%    % Otherwise use coordinates.
%    TargetType =
%        case Target of
%            undefined ->
%                Coordinates;
%            _ ->
%                Target
%        end,
%    action_to_tuple(Player, T, [{Player, Item, TargetType} | Acc]).
%
%register_session(MatchID) ->
%    case gproc:reg({p, l, {match, MatchID}}) of
%        true ->
%            logger:notice("Registered ~p with gproc for Match ~p", [
%                self(),
%                MatchID
%            ]);
%        Other ->
%            logger:warning("Registration failed: ~p", [Other])
%    end.
%  
%deregister_session(MatchID) ->
%    logger:notice("Deregistered ~p from session ~p", [self(), MatchID]),
%    gproc:unreg({p, l, {match, MatchID}}).
%   
%
%pack_match({Id, State, Players, PlayersMax, StartTime, Mode, Extra}) ->
%    #'MatchInfo'{
%        id = Id,
%        state = State,
%        players = Players,
%        players_max = PlayersMax,
%        start_time = StartTime,
%        duration = (erlang:system_time(second) - StartTime),
%        mode = Mode,
%        extra = Extra
%    }.
