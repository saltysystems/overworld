-module(goblet_instance).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

% Public API
-export([
    start/2,
    player_ready/2,
    player_decision/3,
    remove_player/2,
    get_board_state/1,
    get_players/1,
    finalize/1
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
    id :: pos_integer(),
    mobs = [] :: list(),
    playerlist :: list(),
    readyplayers :: list(),
    board :: list(),
    actions = [] :: list()
}).

-type match_id() :: pos_integer().
-type player_name() :: list().

% Public API
-spec start(list(), match_id()) -> {ok, pid()} | ignore | {error, term()}.
start(PlayerList, MatchID) ->
    logger:notice("Starting up state machine for Match ~p", [MatchID]),
    gen_statem:start_link(
        ?SERVER(MatchID),
        ?MODULE,
        {PlayerList, MatchID},
        []
    ).

-spec player_ready(player_name(), match_id()) -> ok.
player_ready(Player, MatchID) ->
    gen_statem:cast(?SERVER(MatchID), {prepare, Player}).

-spec player_decision(player_name(), list(), match_id()) -> ok.
player_decision(Player, Actions, MatchID) ->
    gen_statem:cast(?SERVER(MatchID), {decision, Player, Actions}).

-spec get_board_state(match_id()) -> list().
get_board_state(MatchID) ->
    gen_statem:call(?SERVER(MatchID), board_state).

-spec get_players(match_id()) -> list().
get_players(MatchID) ->
    gen_statem:call(?SERVER(MatchID), player_list).

% Execute this to abruptly end a match and send it to finalizing state.
finalize(MatchID) ->
    gen_statem:cast(?SERVER(MatchID), finalize).

% Only executes if a player has disconnected and their session dies
-spec remove_player(list(), pos_integer()) -> ok.
remove_player(Player, MatchID) ->
    gen_statem:cast(?SERVER(MatchID), {remove_player, Player}).

% Callbacks
init({PlayerList, MatchID}) ->
    M = #match{
        id = MatchID,
        playerlist = PlayerList,
        readyplayers = [],
        board = goblet_game:initialize_board(2, 2, PlayerList)
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
    #match{
        id = ID,
        board = B,
        playerlist = PlayerList,
        readyplayers = Ready
    } = Data,
    ReadyPlayers = [Player | Ready],
    ReadySet = sets:from_list(ReadyPlayers),
    PlayerSet = sets:from_list(PlayerList),
    logger:notice("Player ~p is now ready in Match ~p.", [Player, ID]),
    logger:notice("PlayerSet is ~p", [PlayerSet]),
    logger:notice("ReadySet is ~p", [ReadySet]),
    NextState =
        case PlayerSet == ReadySet of
            true ->
                % All players are ready, reset ready state and move to the
                % next phase. We want a maximum of 20s to go by before we
                % skip over decision and go straight to execute
                logger:notice(
                    "(Prepare) All players are ready. -> Decision"
                ),
                TimerMS = 20000,
                goblet_protocol:match_state_update(
                    pack_tiles(B),
                    [],
                    'DECIDE',
                    PlayerList,
                    [],
                    TimerMS,
                    ID
                ),
                [
                    goblet_protocol:player_state_update(
                        goblet_db:player_shadow(X),
                        ID
                    )
                    || X <- PlayerList
                ],
                TimeOut = {{timeout, decide}, TimerMS, execute},
                {next_state, decision_phase, Data#match{readyplayers = []},
                    [
                        TimeOut
                    ]};
            false ->
                % Not all players are ready, stay in the prepare phase
                % indefinitely
                goblet_protocol:match_state_update(
                    [],
                    [],
                    'PREPARE',
                    PlayerList,
                    ReadyPlayers,
                    0,
                    ID
                ),
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
    #match{id = ID, playerlist = PL, readyplayers = RP, actions = Actions} =
        Data,
    goblet_game:maybe_notify_intent(PlayerActions, ID),
    Ready = [Player | RP],
    RSet = sets:from_list(Ready),
    PSet = sets:from_list(PL),
    logger:notice("(Decision) Player ~p has made a decision.", [Player]),
    Data1 = Data#match{
        actions = [PlayerActions | Actions],
        readyplayers = sets:to_list(RSet)
    },
    case RSet == PSet of
        true ->
            logger:notice("(Decision) All players are ready."),
            % cancel the timer, effectively
            TimeOut = {{timeout, decide}, 0, execute},
            {next_state, decision_phase, Data1, [TimeOut]};
        false ->
            {next_state, decision_phase, Data1}
    end;
decision_phase({timeout, decide}, execute, Data) ->
    % Decisions have timed out, move on
    logger:notice(
        "(Decision) 20000ms have elapsed or all players are ready. Updating match state.."
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
    TimerMS = 10000,
    goblet_protocol:match_state_update(
        pack_tiles(B1),
        Replay,
        'EXECUTE',
        P,
        [],
        TimerMS,
        ID
    ),
    % This is not optimal - create a new shadow for each player after updates
    % and send it back to the player
    [
        goblet_protocol:player_state_update(goblet_db:player_shadow(X), ID)
        || X <- P
    ],
    logger:notice("Done updating match state."),
    % Reset ready players
    Data1 = Data#match{
        readyplayers = [],
        board = B1,
        playerlist = P1,
        mobs = M1,
        actions = A1
    },
    {next_state, execution_phase, Data1, [{state_timeout, TimerMS, decide}]};
decision_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

execution_phase(
    state_timeout,
    decide,
    #match{playerlist = P, id = ID} = Data
) ->
    % Create a new shadow for each player after updates
    % and send it back to the player
    [
        goblet_protocol:player_state_update(goblet_db:player_shadow(X), ID)
        || X <- P
    ],
    logger:notice("(Execute) 10000ms have elapsed. -> Decision"),
    TimerMS = 20000,
    goblet_protocol:match_state_update(
        [],
        [],
        'DECIDE',
        P,
        [],
        TimerMS,
        ID
    ),
    TimeOut = {{timeout, decide}, TimerMS, execute},
    {next_state, decision_phase, Data, [TimeOut]};
execution_phase(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

finish_phase(_EventType, _Timer, #match{playerlist = P, id = ID} = Data) ->
    logger:notice("(Finish) Final timer has executed"),
    goblet_protocol:match_state_update([], [], 'FINISH', P, [], 0, ID),
    {stop, normal, Data}.

pack_tiles(Board) ->
    [
        {X, Y, atom_to_list(Type), [Who]}
        || {_, {X, Y}, Type, _, Who, _} <- Board
    ].

save_and_exit(#match{id = ID}) ->
    %TODO: Save data at the end of match, update a player's inventory or
    %      whatever.
    goblet_lobby:delete_match(ID).

handle_event({call, From}, board_state, #match{board = B} = Data) ->
    {keep_state, Data, [{reply, From, B}]};
handle_event({call, From}, player_list, #match{playerlist = P} = Data) ->
    {keep_state, Data, [{reply, From, P}]};
handle_event(cast, finalize, Data) ->
    TimeOut = {{timeout, final}, 0, final},
    {next_state, finish_phase, Data, [TimeOut]};
% Handle all other events
handle_event(cast, {remove_player, Player}, #match{playerlist = P0} = Data) ->
    P1 = lists:delete(Player, P0),
    {keep_state, Data#match{playerlist = P1}};
handle_event(cast, _EventContent, Data) ->
    {keep_state, Data};
handle_event({call, From}, _EventContent, Data) ->
    {keep_state, Data, [{reply, From, {error, unknown_handler}}]}.

terminate(Reason, _State, Data) ->
    %TODO: Save the player progress
    logger:debug("(Shutdown) Terminate has been called: ~p", [Reason]),
    save_and_exit(Data),
    ok.
