-module(ow_ecs).

-behaviour(gen_server).

-export([start_link/1]).

-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).

%% API
-export([
    add_component/4,
    rm_component/3,
    try_component/3,
    match_component/2,
    add_system/3,
    add_system/2,
    rm_system/2,
    proc/1
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types and Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(world, {
    systems = [] :: [{integer(), mfa() | fun()}],
    ec :: ets:tid()
}).
%-type world() :: #world{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Functions that interact directly with the table
try_component(Name, ID, Table) ->
    case ets:lookup(Table, {ID, Name}) of
        [] ->
            false;
        [{{ID, Name}, _Data}] ->
            true
    end.

match_component(Name, Table) ->
    ets:match(Table, {{'$1', Name}, '$2'}).

% Functions that call out to the gen_server and somehow mutate state
add_component(Name, Data, ID, World) ->
    gen_server:cast(?SERVER(World), {add_component, Name, Data, ID}).

rm_component(Name, ID, World) ->
    gen_server:cast(?SERVER(World), {rm_component, Name, ID}).

add_system(System, World) ->
    add_system(System, 100, World).

add_system(System, Priority, World) ->
    gen_server:cast(?SERVER(World), {add_system, System, Priority}).

rm_system(System, World) ->
    gen_server:cast(?SERVER(World), {rm_system, System}).

proc(World) ->
    gen_server:call(?SERVER(World), proc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Name) ->
    gen_server:start_link(?SERVER(Name), ?MODULE, [], []).

init([]) ->
    World = #world{
        systems = [],
        ec = ets:new(ec, [set])
    },
    {ok, World}.

handle_call(proc, _From, State = #world{systems = S, ec = ECTable}) ->
    % Process all systems in order
    Fun = fun({_Prio, Sys}) ->
        case Sys of
            {M, F, _A} ->
                erlang:apply(M, F, [ECTable]);
            Fun ->
                Fun(ECTable)
        end
    end,
    lists:foreach(Fun, S),
    {reply, ok, State}.

handle_cast({add_component, Name, Data, ID}, State = #world{ec = ECTable}) ->
    ets:insert(ECTable, {{ID, Name}, Data}),
    {noreply, State};
handle_cast({rm_component, Name, ID}, State = #world{ec = ECTable}) ->
    ets:delete(ECTable, {ID, Name}),
    {noreply, State};
handle_cast({add_system, Callback, Prio}, State = #world{systems = S}) ->
    S0 =
        case lists:keytake(Callback, 2, S) of
            false ->
                S;
            {value, _Tuple, SRest} ->
                % Replace the current value instead
                SRest
        end,
    S1 = lists:keysort(1, [{Prio, Callback} | S0]),
    {noreply, State#world{systems = S1}};
handle_cast({rm_system, Callback}, State = #world{systems = S}) ->
    S1 = lists:keydelete(Callback, 2, S),
    {noreply, State#world{systems = S1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
