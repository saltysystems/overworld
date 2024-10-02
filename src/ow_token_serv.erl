-module(ow_token_serv).
-behaviour(gen_server).

-export([
    start_link/0,
    new/1,
    exchange/1
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

-type token() :: binary().

-record(state, {
    tokens = [] :: [{pid(), token()}]
}).

%%----------------------------------------------------------------------------
%% @doc Starts the token server.
%% @end
%%----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% @doc Generates a new token for the given session PID.
%% @end
%%----------------------------------------------------------------------------
-spec new(pid()) -> token().
new(SessionPID) ->
    gen_server:call(?MODULE, {new, SessionPID}).

%%----------------------------------------------------------------------------
%% @doc Exchanges a token for a new one if it matches an existing Session PID.
%% @end
%%----------------------------------------------------------------------------
-spec exchange(token()) -> {pid(), token()} | false | denied.
exchange(Token) ->
    gen_server:call(?MODULE, {exchange, Token}).


%%===========================================================================
%% Callback implementation
%%===========================================================================

init([]) ->
    {ok, #state{}}.

handle_call({new, ID}, _From, #state{tokens = Tokens} = State) ->
    Token = create_token(),
    Tokens1 = [{ID, Token} | Tokens],
    {reply, Token, State#state{tokens = Tokens1}};
handle_call({exchange, Token}, _From, #state{tokens = Tokens} = State) ->
    case lists:keyfind(Token, 2, Tokens) of
        false ->
            {reply, false, State};
        {ID, Token} ->
            % Token matches via pattern matching, give the user a new token
            NewToken = create_token(),
            Tokens1 = lists:keyreplace(ID, 1, Tokens, {ID, NewToken}),
            {reply, {ID, NewToken}, State#state{tokens = Tokens1}};
        {_, _} ->
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


%%===========================================================================
%% Internal functions
%%===========================================================================

-spec create_token() -> token().
create_token() ->
    crypto:strong_rand_bytes(?MAX_BYTES).
