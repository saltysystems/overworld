-module(goblet_lobby).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0]).

-export([
    get_matches/0,
    get_match/1,
    get_match_players/1,
    create_match/3,
    create_match/4,
    join_match/2,
    leave_match/2,
    start_match/1,
    delete_match/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Records representing ephemeral objects, such as matches
-record(goblet_match, {
    id :: match_id(),
    state :: match_state(),
    players = [] :: match_players(),
    players_max :: pos_integer(),
    start_time :: pos_integer(),
    mode :: match_mode(),
    extra = <<>> :: binary()
}).

-type match_players() :: list().
-type match_state() :: atom().
-type match_mode() :: atom().
-type match_id() :: pos_integer().
-type match() :: #goblet_match{}.

%%===================================================================
%% API
%%===================================================================
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%-------------------------------------------------------------------
%% @doc Return a list of matches.
%% @end
%%-------------------------------------------------------------------
-spec get_matches() -> match_players().
get_matches() ->
    gen_server:call(?MODULE, get_matches).

%%-------------------------------------------------------------------
%% @doc Return a single match in tuple format.
%% @end
%%-------------------------------------------------------------------
-spec get_match(integer()) -> {ok, tuple()} | {error, atom()}.
get_match(MatchID) ->
    gen_server:call(?MODULE, {get_match, MatchID}).

%%-------------------------------------------------------------------
%% @doc Return the players in a given game.
%% @end
%%-------------------------------------------------------------------
-spec get_match_players(integer()) -> list() | {error, atom()}.
get_match_players(MatchID) ->
    gen_server:call(?MODULE, {get_match_players, MatchID}).

%%-------------------------------------------------------------------
%% @doc Add a match to the lobby server, return a record of the match
%%      with updated fields (state, ID).
%% @end
%%-------------------------------------------------------------------
-spec create_match(list(), match_mode(), pos_integer()) ->
    {ok, match_id()} | {error, atom()}.
create_match(Player, Mode, MaxPlayers) ->
    create_match(Player, Mode, MaxPlayers, <<>>).

-spec create_match(list(), match_mode(), pos_integer(), binary()) ->
    {ok, match_id()} | {error, atom()}.
create_match(Player, Mode, MaxPlayers, Extra) ->
    gen_server:call(
        ?MODULE,
        {create_match, Player, Mode, MaxPlayers, Extra}
    ).

%%-------------------------------------------------------------------
%% @doc Join a player to a match, so long as it hasn't yet started.
%% @end
%%-------------------------------------------------------------------
-spec join_match(list(), match_id()) -> {ok, tuple()} | {error, atom()}.
join_match(Player, MatchID) ->
    gen_server:call(?MODULE, {join_match, Player, MatchID}).

%%-------------------------------------------------------------------
%% @doc Remove a player from a match
%% @end
%%-------------------------------------------------------------------
-spec leave_match(list(), match_id()) -> ok | {error, atom()}.
leave_match(Player, MatchID) ->
    gen_server:call(?MODULE, {leave_match, Player, MatchID}).

%%-------------------------------------------------------------------
%% @doc Add a match to the lobby server, return a record of the match
%%      with updated fields (state, ID).
%%-------------------------------------------------------------------
-spec start_match(match_id()) -> ok | {error, atom()}.
start_match(MatchID) ->
    gen_server:call(?MODULE, {start_match, MatchID}).

%%-------------------------------------------------------------------
%% @doc Delete the match from the lobby server, if it exists. If
%%      it does not, then do nothing.
%% @end
%%-------------------------------------------------------------------
-spec delete_match(match_id()) -> ok.
delete_match(MatchID) ->
    gen_server:call(?MODULE, {delete_match, MatchID}).

%%===================================================================
%% gen_server Callbacks
%%===================================================================
init([]) ->
    Matches = [],
    NextID = 0,
    Timer = erlang:send_after(1, self(), tick),
    {ok, {NextID, Matches, Timer}}.

handle_call(get_matches, _From, {NextID, Matches, Timer}) ->
    % convert all matches into plain tuples before handing it to the caller
    TupleMatches = [repack_match(X) || X <- Matches],
    {reply, TupleMatches, {NextID, Matches, Timer}};
handle_call({get_match, MatchID}, _From, {NextID, Matches, Timer}) ->
    Match = match_find(MatchID, Matches),
    Reply = match_get(Match),
    {reply, Reply, {NextID, Matches, Timer}};
handle_call({get_match_players, MatchID}, _From, {NextID, Matches, Timer}) ->
    Match = match_find(MatchID, Matches),
    Reply =
        case Match of
            false -> {error, no_such_match};
            Result -> Result#goblet_match.players
        end,
    {reply, Reply, {NextID, Matches, Timer}};
handle_call(
    {create_match, Player, Mode, MaxPlayers, Extra},
    _From,
    {ID, Matches, Timer}
) ->
    MatchState = 'CREATING',
    StartTime = erlang:system_time(second),
    Match = #goblet_match{
        id = ID,
        state = MatchState,
        players = [Player],
        mode = Mode,
        players_max = MaxPlayers,
        start_time = StartTime,
        extra = Extra
    },
    MatchList = [Match | Matches],
    Reply = {ok, ID},
    % Reply with the match id, increment the next available MatchID
    {reply, Reply, {ID + 1, MatchList, Timer}};
handle_call({start_match, MatchID}, _From, {NextID, Matches, Timer}) ->
    Match = match_find(MatchID, Matches),
    {Reply, UpdatedMatches} =
        case maybe_start(Match, Matches) of
            {Error, []} ->
                {Error, Matches};
            {R, U} ->
                {R, U}
        end,
    {reply, Reply, {NextID, UpdatedMatches, Timer}};
handle_call({join_match, Player, MatchID}, _From, {NextID, Matches, Timer}) ->
    Match = match_find(MatchID, Matches),
    {Reply, UpdatedMatch} =
        case maybe_join(Player, Match) of
            {ok, Match1} ->
                {{ok, repack_match(Match1)}, Match1};
            {{error, Error}, _} ->
                {{error, Error}, Match}
        end,
    UpdatedMatches = match_update(UpdatedMatch, Match, Matches),
    {reply, Reply, {NextID, UpdatedMatches, Timer}};
handle_call(
    {leave_match, Player, MatchID},
    _From,
    {NextID, Matches, Timer}
) ->
    Match = match_find(MatchID, Matches),
    {Reply, UpdatedMatch} = maybe_leave(Player, Match),
    UpdatedMatches =
        case UpdatedMatch#goblet_match.players of
            [] ->
                % if no one is in the match after the player leaves, go
                % ahead and delete the original Match record from the
                % Matches list
                logger:notice("Match ~p is empty. Shutting down.", [MatchID]),
                goblet_instance:finalize(MatchID),
                match_del(Match, Matches);
            _ ->
                % otherwise update the Matches list with the latest &
                % greatest record for the Match
                match_update(UpdatedMatch, Match, Matches)
        end,
    {reply, Reply, {NextID, UpdatedMatches, Timer}};
handle_call({delete_match, MatchID}, _From, {NextID, Matches, Timer}) ->
    Match = match_find(MatchID, Matches),
    NewMatches = match_del(Match, Matches),
    % Find any processes still registered to this session in gproc
    Pids = gproc:lookup_pids({p, l, {match, MatchID}}),
    if
        length(Pids) > 0 ->
            logger:notice(
                "Deleting match ~p. ~p clients still attached to this match!",
                [MatchID, Pids]
            );
        true ->
            logger:notice(
                "Deleting match ~p.",
                [MatchID]
            )
    end,
    %TODO: We also need encode a MATCH_LEAVE or MATCH_FINISH message to
    %      every client in this match and let them know that it has poofed.
    %      This should also automagically clean up the gproc state.
    {reply, ok, {NextID, NewMatches, Timer}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, {NextID, Matches, OldTimer}) ->
    % We received a tick event, cancel the old timer
    erlang:cancel_timer(OldTimer),
    % Run the cleanup handler with the current Match information
    NewMatches = cleanup_old_matches(Matches),
    % In miliseconds
    Timer = erlang:send_after(1000 * 60 * 20, self(), tick),
    {noreply, {NextID, NewMatches, Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions that may be subject to change if the backend
% implementation were to change from lists to e.g. ETS
cleanup_old_matches(Matches) ->
    CurrentTime = erlang:system_time(second),
    % Cleanup all matches older than an hour
    % First return all matches that are in Creating state:
    Creating = [X || X <- Matches, X#goblet_match.state == 'CREATING'],
    % Then filter for all matches that are older than 1 hr
    Stale = [
        X
     || X <- Creating, CurrentTime - X#goblet_match.start_time > 3600
    ],
    % Then just delete those matches. This shouldn't be subject to races
    % because the matches are managed entirely by the lobby server.
    logger:notice("Cleaned up ~p old matches", [length(Stale)]),
    M1 = Matches -- Stale,
    % Clean up matches in any state that have 0 players in them and older
    % than 5 minutes.
    Empty = [
        X
     || X <- M1,
        length(X#goblet_match.players) =:= 0,
        CurrentTime - X#goblet_match.start_time > 300
    ],
    M1 -- Empty.

match_find(MatchID, Matches) ->
    lists:keyfind(MatchID, #goblet_match.id, Matches).

match_get(false) ->
    {error, no_such_match};
match_get(Match) ->
    {ok, repack_match(Match)}.

match_del(false, Matches) ->
    % simply return the existing list of matches
    Matches;
match_del(Match, Matches) ->
    MatchID = Match#goblet_match.id,
    lists:keydelete(MatchID, #goblet_match.id, Matches).

match_update(_UpdatedMatch, false, Matches) ->
    Matches;
match_update(false, _Match, Matches) ->
    Matches;
match_update(UpdatedMatch, Match, Matches) ->
    lists:keyreplace(
        Match#goblet_match.id,
        #goblet_match.id,
        Matches,
        UpdatedMatch
    ).

maybe_leave(_Player, false) ->
    {ok, false};
maybe_leave(Player, Match) ->
    if
        Match#goblet_match.state =:= 'PLAYING' ->
            goblet_instance:remove_player(Player, Match#goblet_match.id);
        true ->
            ok
    end,
    Players = Match#goblet_match.players,
    UpdatedPlayers = lists:delete(Player, Players),
    UpdatedMatch = Match#goblet_match{players = UpdatedPlayers},
    {ok, UpdatedMatch}.

maybe_join(_Player, false) ->
    {{error, no_such_match}, false};
maybe_join(_Player, Match) when
    length(Match#goblet_match.players) >= Match#goblet_match.players_max
->
    {{error, match_full}, Match};
maybe_join(Player, Match) ->
    case
        goblet_util:run_checks([
            fun() -> is_valid_player(Player) end,
            fun() -> is_unstarted_match(Match) end,
            fun() -> is_not_in_match(Player, Match) end
        ])
    of
        ok ->
            Players = Match#goblet_match.players,
            {ok, Match#goblet_match{players = [Player | Players]}};
        Error ->
            {Error, Match}
    end.

is_unstarted_match(M) when M#goblet_match.state == 'CREATING' ->
    ok;
is_unstarted_match(_M) ->
    {error, already_started}.

is_valid_player(Player) ->
    case goblet_db:is_valid_player(Player) of
        true -> ok;
        false -> {error, invalid_player}
    end.

is_not_in_match(Player, Match) ->
    case lists:member(Player, Match#goblet_match.players) of
        false -> ok;
        true -> {error, already_joined}
    end.

maybe_start(false, Matches) ->
    {{error, no_such_match}, Matches};
maybe_start(Match, Matches) when length(Match#goblet_match.players) == 0 ->
    {{error, empty_match}, Matches};
maybe_start(Match, Matches) ->
    supervisor:start_child(goblet_instance_sup, [
        Match#goblet_match.players,
        Match#goblet_match.id
    ]),
    UpdatedMatch = Match#goblet_match{state = 'PLAYING'},
    UpdatedMatches = match_update(UpdatedMatch, Match, Matches),
    {ok, UpdatedMatches}.

repack_match(Match) ->
    {
        Match#goblet_match.id,
        Match#goblet_match.state,
        Match#goblet_match.players,
        Match#goblet_match.players_max,
        Match#goblet_match.start_time,
        Match#goblet_match.mode,
        Match#goblet_match.extra
    }.

%%===================================================================
%% Unit Tests
%%===================================================================

get_matches_empty_test() ->
    Matches = goblet_lobby:get_matches(),
    ?assertEqual([], Matches).

add_match_test() ->
    % Note that we don't do any validation here-
    Name = "Chester Tester",
    {ok, MatchID} = create_match(Name, 'DEFAULT', 6),
    {ok, ConfirmedMatch} = get_match(MatchID),
    {Id, State, Players, PlayerMax, _StartTime, Mode, _Extra} =
        ConfirmedMatch,
    ?assertEqual('CREATING', State),
    ?assertEqual([Name], Players),
    ?assertEqual(6, PlayerMax),
    ?assertEqual('DEFAULT', Mode),
    Matches1 = get_matches(),
    ?assertEqual([ConfirmedMatch], Matches1),
    % we are bad at unit tests. this one relies on the first test
    Result = delete_match(Id),
    ?assertEqual(ok, Result),
    Matches2 = get_matches(),
    ?assertEqual([], Matches2).

join_leave_match_test() ->
    % Create a player
    Name = "Chester Tester",
    Email = "Test@localhost.localdomain",
    _Resp0 = goblet_db:create_account(Email, "test"),
    _Resp1 = goblet_game:new_player(
        Name,
        ["#c0ffee", "#ffffff", "#000000"],
        [1, 3],
        'DESTROYER',
        Email
    ),
    {ok, MatchID} = create_match(Name, 'DEFAULT', 6),
    {ok, M} = get_match(MatchID),
    {MatchID, _S, Players, _PM, _ST, _M, _E} = M,
    ?assertEqual([Name], Players),
    % Try to join again, just for fun
    {error, Error} = join_match(Name, MatchID),
    ?assertEqual(Error, already_joined),
    % attempt to leave the match
    ?assertEqual(ok, leave_match(Name, MatchID)),
    ?assertEqual({error, no_such_match}, get_match(MatchID)),
    ?assertEqual(ok, goblet_db:delete_player(Name, Email)),
    ?assertEqual(ok, goblet_db:delete_account(Email)).

start_match_test() ->
    % Create a player
    Name = "Chester Tester",
    Email = "Test@localhost.localdomain",
    _Resp0 = goblet_db:create_account(Email, "test"),
    _Resp1 = goblet_game:new_player(
        Name,
        ["#c0ffee", "#ffffff", "#000000"],
        [1, 3],
        'DESTROYER',
        Email
    ),
    {ok, MatchId} = create_match(Name, 'DEFAULT', 6),
    {ok, M} = get_match(MatchId),
    {MatchId, _S, _P, _PM, _ST, _M, _E} = M,
    % This is validated by the session
    ok = start_match(MatchId),
    % Can we leave a match if its running?
    ?assertEqual(ok, leave_match(Name, MatchId)),
    ?assertEqual({error, no_such_match}, get_match(MatchId)),
    ?assertEqual(ok, goblet_db:delete_player(Name, Email)),
    ?assertEqual(ok, goblet_db:delete_account(Email)).

repack_match_test() ->
    Id = 1,
    State = 'CREATING',
    Players = [],
    PlayersMax = 6,
    StartTime = erlang:system_time(second),
    Mode = 'DEFAULT',
    Extra = <<"ABCD">>,
    M = #goblet_match{
        id = Id,
        state = State,
        players = Players,
        players_max = PlayersMax,
        start_time = StartTime,
        mode = Mode,
        extra = Extra
    },
    ?assertEqual(
        {Id, State, Players, PlayersMax, StartTime, Mode, Extra},
        repack_match(M)
    ).
