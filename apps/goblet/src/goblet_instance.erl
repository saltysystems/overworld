-module(goblet_instance).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

% Public API
-export([start/2, 
        player_ready/2, 
        player_decision/3, 
        get_board_state/1, 
        get_players/1
]).

% Required gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).

% State Functions
% Do not use directly
-export([
    prepare_phase/3,
    decision_phase/3,
    execution_phase/3
    %finish_phase/4,
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

get_players(MatchID) ->
    gen_statem:call(?SERVER(MatchID), player_list).

% Callbacks
init({PlayerList, MatchID}) ->
    M = #match{
        id = MatchID,
        playerlist = PlayerList,
        readyplayers = [],
        board = goblet_game:initialize_board(1, 1, PlayerList)
    },
    {ok, prepare_phase, M}.

callback_mode() ->
    state_functions.

% In prepare stage, we wait for all Players to confirm that they are ready.
prepare_phase(
    cast,
    {prepare, Player},
    Data
) ->
    #match{id = ID, playerlist = PlayerList, readyplayers = Ready} = Data,
    ReadyPlayers = [Player | Ready],
    ReadySet = sets:from_list(ReadyPlayers),
    PlayerSet = sets:from_list(PlayerList),
    logger:notice("Player ~p is now ready in Match ~p.", [Player, ID]),
    NextState =
        case PlayerSet == ReadySet of
            true ->
                % All players are ready, reset ready state and move to the
                % next phase. We want a maximum of 20s to go by before we
                % skip over decision and go straight to execute
                logger:notice(
                    "(Prepare) All players are ready. -> Decision"
                ),
                TimeOut = {{timeout, decide}, 20000, execute},
                {next_state, decision_phase, Data#match{readyplayers = []},
                    [
                        TimeOut
                    ]};
            false ->
                % Not all players are ready, stay in the prepare phase
                % indefinitely
                {next_state, prepare_phase, Data#match{
                    readyplayers = ReadyPlayers
                }}
        end,
    logger:notice("(Prepare) Match ~p: ~p of ~p players are ready", [
        ID,
        sets:size(ReadySet),
        sets:size(PlayerSet)
    ]),
    NextState;
prepare_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

% In the decision stage, we accept decisions from players until the timeout is
% reached. Then we move to Execute phase.
decision_phase(
    cast,
    {decision, Player, PlayerActions},
    Data
) ->
    #match{playerlist = PL, readyplayers = RP, actions = Actions} = Data,
    % TODO: Validate action list
    Ready = [Player | RP],
    RSet = sets:from_list(RP),
    PSet = sets:from_list(PL),
    logger:notice("(Decision) Player ~p has made a decision.", [Player]),
    % Update the match state with the player's decision
    Data1 = Data#match{actions = [PlayerActions | Actions]},
    NextState =
        case RSet == PSet of
            true ->
                % All players have made their decision
                % Cancel the decision timer by setting it to infinity and
                % move onto execute state
                logger:notice(
                    "(Decision) All players are ready. -> Execution"
                ),
                TimeOut = {{timeout, decide}, infinity, execute},
                {next_state, execution_phase,
                    Data1#match{readyplayers = []}, [
                        TimeOut
                    ]};
            false ->
                {next_state, decision_phase, Data1#match{
                    readyplayers = Ready
                }}
        end,
    logger:notice("(Decision) Ready players are: ~p", [Ready]),
    NextState;
decision_phase({timeout, decide}, execute, Data) ->
    % Decisions have timed out, move on
    logger:notice("(Decision) 20000ms have elapsed. -> Execution"),
    {next_state, execution_phase, Data, [{state_timeout, 10000, decide}]};
decision_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

% In the execution phase, we group actions, calculate the results, and give
% the clients a bit of time to play animations. Then we expect to hear the
% clients have finished with their animations, or we time out after some
% generous time period (60s ?). We check to see if the match is over, and go
% to the Finish phase if it is. Otherwise, back to Decision.
execution_phase(
    state_timeout,
    decide,
    Data
) ->
    #match{id = ID, board = B0, actions = A, playerlist = P} = Data,
    B1 = goblet_game:calculate_round(A, B0),
    goblet_protocol:match_state_update(B1, [], 'EXECUTE', P, [], ID),
    logger:notice("(Execute) 10000ms have elapsed. -> Decison"),
    TimeOut = {{timeout, decide}, 20000, execute},
    {next_state, decision_phase, Data, [TimeOut]};
execution_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

handle_event({call, From}, board_state, #match{board = B} = Data) ->
    {keep_state, Data, [{reply, From, B}]};
handle_event({call, From}, player_list, #match{playerlist = P} = Data) ->
    {keep_state, Data, [{reply, From, P}]};
% Handle all other events
handle_event(cast, _EventContent, Data) ->
    {keep_state, Data};
handle_event({call, From}, _EventContent, Data) ->
    {keep_state, Data, [{reply, From, {error, unknown_handler}}]}.

terminate(_Reason, _State, _Data) ->
    %TODO: Save the player progress
    ok.
