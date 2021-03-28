-module(goblet_instance).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

% internal functions ackchually
-export([phases/1, normalize_actions/1]).

% Public API
-export([start/2, player_ready/2, player_decision/3]).

% Callbacks
-export([init/1, callback_mode/0, terminate/3]).

% State Functions
% Do not use directly
-export([
    prepare/3,
    decision/3,
    execution/3
    %finish/4
]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

-record(action, {ap, name, from, target}).
-record(match, {id, playerlist, readyplayers, board}).

% Public API
start(PlayerList, MatchID) ->
    logger:notice("Starting up state machine for Match ~p", [MatchID]),
    gen_statem:start_link(
        ?SERVER(MatchID),
        ?MODULE,
        {PlayerList, MatchID},
        []
    ).

player_ready(Player, MatchID) ->
    gen_statem:cast(?SERVER(MatchID), {prepare, Player}).

player_decision(Player, Actions, MatchID) ->
    gen_statem:cast(?SERVER(MatchID), {decision, Player, Actions}).

% Callbacks
init({PlayerList, MatchID}) ->
    M = #match{
        id = MatchID,
        playerlist = PlayerList,
        readyplayers = [],
        board = goblet_game:initialize_board(1, 1, PlayerList)
    },
    {ok, prepare, M}.

callback_mode() ->
    state_functions.

% State Functions

% In prepare stage, we wait for all Players to confirm that they are ready.
prepare(
    cast,
    {prepare, Player},
    #match{id = _ID, playerlist = PlayerList, readyplayers = Ready} = Data
) ->
    ReadyPlayers = [Player | Ready],
    % TODO: Validate that Ready is a subset of Player
    ReadySet = sets:from_list(ReadyPlayers),
    PlayerSet = sets:from_list(PlayerList),
    logger:notice("Player ~p is now ready.", [Player]),
    NextState =
        case PlayerSet == ReadySet of
            true ->
                % All players are ready, reset ready state and move to the
                % next phase. We want a maximum of 20s to go by before we
                % skip over decision and go straight to execute
                logger:notice(
                    "All players are ready, move on to decision state"
                ),
                TimeOut = {{timeout, decide}, 20000, execute},
                {next_state, decision, Data#match{readyplayers = []}, [
                    TimeOut
                ]};
            false ->
                % Wait for all players to be ready
                % TODO: Implement timeout
                {next_state, prepare, Data#match{
                    readyplayers = ReadyPlayers
                }}
        end,
    logger:notice("Ready players are:~p", [ReadyPlayers]),
    
    NextState;
prepare(EventType, EventContent, Data) ->
    logger:warning("Got a bad event sent to prepare"),
    handle_event(EventType, EventContent, Data).

% In the decision stage, we accept decisions from players until the timeout is
% reached. Then we move to Execute phase.
decision(
    cast,
    {decision, Player, Actions},
    #match{playerlist = PL, readyplayers = RP} = Data
) ->
    % TODO: Validate action list
    Ready = [Player | RP],
    RSet = sets:from_list(RP),
    PSet = sets:from_list(PL),
    logger:notice("Player ~p has made a decision.", [Player]),
    NextState =
        case RSet == PSet of
            true ->
                % All players have made their decision
                % Cancel the decision timer by setting it to infinity and move
                % onto execute state
                logger:notice(
                    "All players are ready, move on to execution state"
                ),
                TimeOut = {{timeout, decide}, infinity, execute},
                {next_state, execution, Data#match{readyplayers = []}, [
                    TimeOut
                ]};
            false ->
                {next_state, decision, Data#match{readyplayers = Ready}}
        end,
    logger:notice("Ready players are: ~p", [Ready]),
    NextState;
decision({timeout, decide}, execute, Data) ->
    % Decisions have timed out, move on
    logger:notice("20000ms has expired - force move to execution state"),
    {next_state, execution, Data, [{state_timeout, 10000, decide}]};
decision(EventType, EventContent, Data) ->
    logger:warning("Got a bad event sent to decision"),
    handle_event(EventType, EventContent, Data).

% In the execution phase, we group actions, calculate the results, and give
% the clients a bit of time to play animations. Then we expect to hear the
% clients have finished with their animations, or we time out after some
% generous time period (60s ?). We check to see if the match is over, and go
% to the Finish phase if it is. Otherwise, back to Decision.
%execution(cast, {execution, Player, Actions}, Data) ->
%    NewData = Data, % calculate results here
%    {next_state, decision, NewData, [{state_timeout, 5000, decide}]};
execution(state_timeout, decide, Data) ->
    % calculate results here
    NewData = Data,
    logger:notice("10000ms has passed for animations, back to decision"),
    TimeOut = {{timeout, decide}, 20000, execute},
    {next_state, decision, Data, [TimeOut]};
execution(EventType, EventContent, Data) ->
    logger:warning("Got a bad event sent to execution"),
    handle_event(EventType, EventContent, Data).

% In the finish phase, we instruct the clients to wrap up, show them results, and we shut down or wait to be shut down by our supervisor.

handle_event(EventType, EventContent, Data) ->
    {keep_state, Data}.

terminate(_Reason, _State, _Data) ->
    %TODO: Save the player progress
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Internal Functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalize_actions(ActionList) ->
    % Take a list of actions for a player, e.g.
    %    [{1,shoot}, {2, move, {4, laz0r}]
    % and then normalize it such that the AP costs are translated into Phases:
    %  [{1,shoot}, {3, move}, {7, laz0r}]
    normalize_actions(ActionList, []).

normalize_actions([], Acc) ->
    lists:reverse(Acc);
normalize_actions([H | T], []) ->
    normalize_actions(T, [H]);
normalize_actions([H | T], [AccH | AccT]) when is_record(H, action) ->
    MP = H#action.ap + AccH#action.ap,
    MPList = H#action{ap = MP},
    normalize_actions(T, [MPList | [AccH | AccT]]);
normalize_actions([H | T], [AccH | AccT]) ->
    % H = 1, T = [2,3,4] ; AccH = 1, AccT = []
    NewList = H + AccH,
    normalize_actions(T, [NewList | [AccH | AccT]]).

% The ActionList must already do the math needed to handle this
phases(ActionList) ->
    % Get the largest key
    % AP must be the first item in the record, hence 2nd key
    S = lists:keysort(2, ActionList),
    % Get the last element of the list
    MaxPhase = lists:last(S),
    % Recurse through the phases, pairing up matching keys into the same phase
    % til we hit 0.
    group_phases(MaxPhase#action.ap, S).

group_phases(N, KeyList) ->
    group_phases(N, KeyList, []).

group_phases(N, _KeyList, Acc) when N == 0 ->
    Acc;
group_phases(N, KeyList, Acc) ->
    %PhaseGroup = [ X || {_, N1, _, _} = X <- KeyList, N == N1],
    PhaseGroup = [X || #action{ap = N1} = X <- KeyList, N == N1],
    case PhaseGroup of
        [] ->
            group_phases(N - 1, KeyList, Acc);
        _ ->
            group_phases(N - 1, KeyList, [PhaseGroup | Acc])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Test                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalize_actions_test() ->
    Actions = [
        #action{name = shoot, ap = 2, from = "player1", target = "player2"},
        #action{name = move, ap = 1, from = "player1", target = {0, 0}},
        #action{name = laz0r, ap = 6, from = "player2", target = "player1"}
    ],
    % Convert AP to Phases
    MP = normalize_actions(Actions),
    Last = lists:last(MP),
    ?assertEqual(9, Last#action.ap).
%?assertEqual(9, Last#action.mp).

phases_test() ->
    P1_Actions = [
        #action{name = shoot, ap = 2, from = "player1", target = "player2"},
        #action{name = move, ap = 1, from = "player1", target = {0, 0}},
        #action{name = laz0r, ap = 6, from = "player1", target = "player2"}
    ],
    P2_Actions = [
        #action{name = move, ap = 1, from = "player2", target = {1, 1}},
        #action{name = shoot, ap = 2, from = "player2", target = "player1"},
        #action{
            name = shield,
            ap = 3,
            from = "player2",
            target = "player1"
        },
        #action{
            name = missile,
            ap = 3,
            from = "player2",
            target = "player1"
        }
    ],
    % Convert AP to Phases
    MP = normalize_actions(P1_Actions),
    MP2 = normalize_actions(P2_Actions),
    Collected = MP ++ MP2,
    % Group the phases
    phases(Collected).
