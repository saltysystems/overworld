-module(ow_token_serv).
-behaviour(gen_server).

-export([
    start_link/0,
    new/1,
    exchange/2
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

-define(MAX_BYTES, 16).

-record(state, {
    tokens = []
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
new(ID) ->
    gen_server:call(?MODULE, {new, ID}).
exchange(ID, Token) ->
    gen_server:call(?MODULE, {exchange, ID, Token}).

init([]) ->
    {ok, #state{}}.

handle_call({new, ID}, _From, #state{tokens = Tokens} = State) ->
    Token = create_token(),
    Tokens1 = [{ID, Token} | Tokens],
    {reply, Token, State#state{tokens = Tokens1}};
handle_call({exchange, ID, Token}, _From, #state{tokens = Tokens} = State) ->
    case lists:keyfind(ID, 1, Tokens) of
        false ->
            {reply, false, State};
        {ID, Token} ->
            % Token matches via pattern matching, give the user a new token
            NewToken = create_token(),
            Tokens1 = lists:keyreplace(ID, 1, Tokens, {ID, NewToken}),
            {reply, NewToken, State#state{tokens = Tokens1}};
        {ID, _} ->
            {reply, denied, State}
    end;
handle_call(_, _, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
create_token() ->
    crypto:strong_rand_bytes(?MAX_BYTES).
