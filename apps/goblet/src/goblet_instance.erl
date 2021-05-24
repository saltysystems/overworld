-module(goblet_instance).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

% Public API
-export([
    start/2,
    player_ready/2,
    player_decision/3,
    get_board_state/1,
    get_players/1,
    stop/1
]).

% Required gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).

% State Functions
% Do not use directly
-export([
    prepare_phase/3,
    decision_phase/3,
    execution_phase/3,
    finish_phase/3
]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

%TODO: Generate some mobs!
-record(match, {
    id,
    mobs = [],
    playerlist,
    readyplayers,
    board,
    actions = []
}).

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

% Execute this to abruptly end a match and send it to finalizing state.
stop(MatchID) ->
    gen_statem:cast(?SERVER(MatchID), stop).

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
    #match{playerlist = _PL, readyplayers = RP, actions = Actions} = Data,
    Ready = [Player | RP],
    RSet = sets:from_list(Ready),
    %PSet = sets:from_list(PL),
    logger:notice("(Decision) Player ~p has made a decision.", [Player]),
    % Update the match state with the player's decision
    Data1 = Data#match{actions = [PlayerActions | Actions]},
    {next_state, decision_phase, Data1#match{
        readyplayers = sets:to_list(RSet)
    }};
decision_phase({timeout, decide}, execute, Data) ->
    % Decisions have timed out, move on
    logger:notice(
        "(Decision) 20000ms have elapsed. Updating match state.."
    ),
    #match{id = ID, board = B0, actions = A, playerlist = P, mobs = M} =
        Data,
    % Create a "shadow" of the player that contains a subset of the
    % information needed to do round calculations and may go through many
    % perturbations before settling. This should save potentially many
    % database calls by working only in process memory, and make the
    % transaction atomic.
    PlayerShadows = [goblet_db:player_shadow(X) || X <- P],
    logger:debug("Calculating round with the following parameters..."),
    logger:debug("Mobs: ~p, Players: ~p", [M, PlayerShadows]),
    logger:debug("Actions: ~p, Board: ~p", [A, B0]),
    {A1, M1, P1, B1, Replay} = goblet_game:calculate_round(
        {A, M, PlayerShadows, B0}
    ),
    % need to unpack the tiles into a plain tuple. still very unhappy with
    % this
    Tiles = [
        {X, Y, atom_to_list(Type), [Who]}
     || {_, {X, Y}, Type, _, Who, _} <- B1
    ],
    goblet_protocol:match_state_update(Tiles, Replay, 'EXECUTE', P, [], ID),
    logger:notice("Done updating match state."),
    % Reset ready players
    Data1 = Data#match{
        readyplayers = [],
        board = B1,
        playerlist = P1,
        mobs = M1,
        actions = A1
    },
    {next_state, execution_phase, Data1, [{state_timeout, 10000, decide}]};
decision_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

execution_phase(state_timeout, decide, #match{playerlist=P, id=ID} = Data) ->
    logger:notice("(Execute) 10000ms have elapsed. -> Decision"),
    goblet_protocol:match_state_update([], [], 'DECIDE', P, [], ID),
    TimeOut = {{timeout, decide}, 20000, execute},
    {next_state, decision_phase, Data, [TimeOut]};
execution_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

finish_phase(state_timeout, Timer, #match{playerlist=P, id=ID} = Data) ->
    % We just latch onto any running timer and wait for it to fire
    logger:notice("(Finish) Final timer has executed", [Timer]),
    goblet_protocol:match_state_update([], [], 'FINISH', P, [], ID),
    Status = save_and_exit(Data),
    {stop, Status}.

save_and_exit(#match{id=ID}) ->
    %TODO: Save data at the end of match, update a player's inventory or
    %      whatever.
    goblet_lobby:delete_match(ID),
    normal.

handle_event({call, From}, board_state, #match{board = B} = Data) ->
    {keep_state, Data, [{reply, From, B}]};
handle_event({call, From}, player_list, #match{playerlist = P} = Data) ->
    {keep_state, Data, [{reply, From, P}]};
handle_event(cast, stop, Data) ->
    {next_state, finish_phase, Data};
% Handle all other events
handle_event(cast, _EventContent, Data) ->
    {keep_state, Data};
handle_event({call, From}, _EventContent, Data) ->
    {keep_state, Data, [{reply, From, {error, unknown_handler}}]}.

terminate(_Reason, _State, Data) ->
    %TODO: Save the player progress
    logger:notice("(Finish) Terminate has been called"),
    case save_and_exit(Data) of
        normal -> ok
    end.
