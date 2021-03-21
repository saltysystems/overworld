-module(goblet_lobby).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0]).

-export([
    get_matches/0,
    create_match/2,
    create_match/3,
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

-include("goblet_database.hrl").

%%===================================================================
%% API
%%===================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%%-------------------------------------------------------------------
%% @doc Return a list of matches in record format
%%-------------------------------------------------------------------
-spec get_matches() -> list().
get_matches() ->
    gen_server:call(?MODULE, get_matches).

%%-------------------------------------------------------------------
%% @doc Add a match to the lobby server, return a record of the match
%%      with updated fields (state, ID).
%%-------------------------------------------------------------------
-spec create_match(atom(), pos_integer()) -> {ok, tuple()} | {error, atom()}.
create_match(Mode, MaxPlayers) ->
    create_match(Mode, MaxPlayers, <<>>).

-spec create_match(atom(), pos_integer(), binary()) -> {ok, tuple()} | {error, atom()}.
create_match(Mode, MaxPlayers, Extra) ->
    gen_server:call(?MODULE, {create_match, Mode, MaxPlayers, Extra}).

%%-------------------------------------------------------------------
%% @doc Add a match to the lobby server, return a record of the match
%%      with updated fields (state, ID).
%%-------------------------------------------------------------------
-spec start_match(pos_integer()) -> {ok, tuple()} | {error, tuple()}.
start_match(MatchID) ->
    gen_server:call(?MODULE, {start_match, MatchID}).

%%-------------------------------------------------------------------
%% @doc Delete the match from the lobby server, if it exists. If
%%      it does not, then do nothing.
%%-------------------------------------------------------------------
-spec delete_match(pos_integer()) -> ok.
delete_match(MatchInfo) ->
    gen_server:call(?MODULE, {delete_match, MatchInfo}).

%%===================================================================
%% gen_server Callbacks
%%===================================================================
init([]) ->
    Matches = [],
    NextID = 0,
    {ok, {NextID, Matches}}.

handle_call(get_matches, _From, {NextID, Matches}) ->
    {reply, Matches, {NextID, Matches}};
handle_call({create_match, Mode, MaxPlayers, Extra}, _From, {ID, Matches}) ->
    MatchState = 'CREATING',
    StartTime = erlang:system_time(),
    Match = #goblet_match{
        id = ID,
        state = MatchState,
        mode = Mode,
        players_max = MaxPlayers,
        start_time = StartTime,
        extra = Extra
    },
    MatchList = [Match | Matches],
    Reply = {ok, Match},
    % Reply with the match list, increment the next available MatchID
    {reply, Reply, {ID + 1, MatchList}};
handle_call({start_match, MatchID}, _From, {NextID, Matches}) ->
    Match = match_find(MatchID, Matches),
    {Reply, UpdatedMatches} =
        case maybe_start(Match, Matches) of
            {Error, []} ->
                {Error, Matches};
            {R, U} ->
                {R, U}
        end,
    {reply, Reply, {NextID, UpdatedMatches}};
handle_call({delete_match, MatchID}, _From, {NextID, Matches}) ->
    Match = match_find(MatchID, Matches),
    NewMatches = match_del(Match, Matches),
    {reply, ok, {NextID, NewMatches}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions that may be subject to change if the backend
% implementation were to change from lists to e.g. ETS

match_find(MatchID, Matches) ->
    lists:keyfind(MatchID, #goblet_match.id, Matches).

match_del(Match, Matches) when Match == false ->
    % simply return the existing list of matches
    Matches;
match_del(Match, Matches) ->
    MatchID = Match#goblet_match.id,
    lists:keydelete(MatchID, #goblet_match.id, Matches).

maybe_start(Match, Matches) when Match == false ->
    {{error, no_such_match}, Matches};
maybe_start(Match, Matches) when length(Match#goblet_match.players) == 0 ->
    {{error, empty_match}, Matches};
maybe_start(Match, Matches) ->
    supervisor:start_child(goblet_instance_sup, [Match#goblet_match.players, Match#goblet_match.id]),
    UpdatedMatch = Match#goblet_match{state = 'PLAYING'},
    lists:keyreplace(Match#goblet_match.id, #goblet_match.id, Matches, UpdatedMatch),
    {ok, started}.
