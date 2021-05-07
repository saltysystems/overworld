-module(goblet_instance).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

% Public API
-export([start/2, player_ready/2, player_decision/3, get_board_state/1]).

% Callbacks
-export([init/1, callback_mode/0, terminate/3]).

% State Functions
% Do not use directly
-export([
    prepare/3,
    decision/3,
    execution/3,
    %finish/4,
    board_state/3
]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

-record(match, {id, playerlist, readyplayers, board, actions = []}).

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

get_board_state(MatchID) ->
    gen_statem:call(?SERVER(MatchID), board_state).

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
board_state({call, From}, board_state, Data) ->
    Board = Data#match.board,
    {keep_state, {reply, From, Board}}.

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
    {decision, Player, PlayerActions},
    #match{playerlist = PL, readyplayers = RP, actions = Actions} = Data
) ->
    % TODO: Validate action list
    Ready = [Player | RP],
    RSet = sets:from_list(RP),
    PSet = sets:from_list(PL),
    logger:notice("Player ~p has made a decision.", [Player]),
    % Update the match state with the player's decision
    Data1 = Data#match{actions = [PlayerActions | Actions]},
    NextState =
        case RSet == PSet of
            true ->
                % All players have made their decision
                % Cancel the decision timer by setting it to infinity and
                % move onto execute state
                logger:notice(
                    "All players are ready, move on to execution state"
                ),
                TimeOut = {{timeout, decide}, infinity, execute},
                {next_state, execution, Data1#match{readyplayers = []}, [
                    TimeOut
                ]};
            false ->
                {next_state, decision, Data1#match{readyplayers = Ready}}
        end,
    logger:notice("Ready players are: ~p", [Ready]),
    NextState;
decision({timeout, decide}, execute, Data) ->
    % Decisions have timed out, move on
    logger:notice("10000ms has expired - force move to execution state"),
    {next_state, execution, Data, [{state_timeout, 10000, decide}]};
decision(EventType, EventContent, Data) ->
    logger:warning("Got a bad event sent to decision"),
    handle_event(EventType, EventContent, Data).

% In the execution phase, we group actions, calculate the results, and give
% the clients a bit of time to play animations. Then we expect to hear the
% clients have finished with their animations, or we time out after some
% generous time period (60s ?). We check to see if the match is over, and go
% to the Finish phase if it is. Otherwise, back to Decision.
execution(
    state_timeout,
    decide,
    #match{id = ID, board = B0, actions = A, playerlist = P} = Data
) ->
    B1 = goblet_game:calculate_round(A, B0),
    goblet_protocol:match_state_update(B1, [], 'EXECUTE', P, [], ID),
    logger:notice("60000ms has passed for animations, back to decision"),
    TimeOut = {{timeout, decide}, 20000, execute},
    {next_state, decision, Data, [TimeOut]};
execution(EventType, EventContent, Data) ->
    logger:warning("Got a bad event sent to execution"),
    handle_event(EventType, EventContent, Data).

% In the finish phase, we instruct the clients to wrap up, show them results, and we shut down or wait to be shut down by our supervisor.

handle_event(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

terminate(_Reason, _State, _Data) ->
    %TODO: Save the player progress
    ok.
