-module(goblet_lobby).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start/0, stop/0]).

-export([
    get_matches/0, 
    create_match/3, 
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
-spec create_match(atom(),pos_integer(),binary()) -> {ok, tuple()}.
create_match(Mode, MaxPlayers, Extra) ->
    gen_server:call(?MODULE, {create_match, Mode, MaxPlayers, Extra}).

%%-------------------------------------------------------------------
%% @doc Delete the match from the lobby server, if it exists. If
%%      it does not, then do nothing.
%%-------------------------------------------------------------------
-spec delete_match(tuple()) -> ok.
delete_match(MatchInfo) ->
    gen_server:call(?MODULE, {delete_match, MatchInfo}).

%%===================================================================
%% gen_server Callbacks
%%===================================================================
init([]) ->
    MatchList = [],
    NextID = 0,
    {ok, {NextID, MatchList}}.

handle_call(get_matches, _From, {ID, MatchList}) ->
    {reply, MatchList, {ID, MatchList}};
handle_call({create_match, Mode, MaxPlayers, Extra}, _From, {ID, Matches}) ->
    MatchState = 'CREATING',
    StartTime = erlang:system_time(),
    Match = #goblet_match{id = ID, 
                            state = MatchState, 
                            mode = Mode,
                            players_max = MaxPlayers,
                            start_time = StartTime,
                            extra = Extra},
    MatchList = [Match | Matches],
    Reply = {ok, Match},
    % Reply with the match list, increment the next available MatchID
    {reply, Reply, {ID + 1, MatchList}};
handle_call({delete_match, MatchInfo}, _From, {NextID, Matches}) ->
    ID = MatchInfo#goblet_match.id,
    NewMatches = lists:keydelete(ID, #goblet_match.id, Matches),
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
