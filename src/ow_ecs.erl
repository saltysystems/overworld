-module(ow_ecs).

-behaviour(gen_server).

-define(SERVER(World),
    {via, gproc, {n, l, {?MODULE, World}}}
).

%% API
-export([start/1, start_link/1, stop/1]).
-export([
    new_entity/2,
    rm_entity/2,
    entity/2,
    entities/1,
    add_component/4,
    add_components/3,
    del_component/3,
    del_components/3,
    try_component/3,
    match_component/2,
    match_components/2,
    foreach_component/3,
    add_system/3,
    add_system/2,
    del_system/2,
    world/1,
    proc/1,
    proc/2,
    to_map/1,
    get/3,
    get/2,
    take/3,
    take/2,
    query/1
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
    name :: term(),
    systems = [] :: [{term(), system()}],
    entities :: ets:tid(),
    components :: ets:tid()
}).

-opaque query() :: {ets:tid(), ets:tid(), any()}.
-export_type([query/0]).

-type world() :: #world{}.
-type component() :: {term(), term()}.
-type entity() :: {term(), [component()]}.
-export_type([entity/0]).
-type system() :: {mfa() | fun()}.
-type id() :: integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(World) ->
    gen_server:start(?SERVER(World), ?MODULE, [World], []).

start_link(World) ->
    gen_server:start_link(?SERVER(World), ?MODULE, [World], []).

stop(World) ->
    gen_server:stop(?SERVER(World)).

% Get the query object from the world name
-spec query(world()) -> query().
query(World) ->
    gen_server:call(?SERVER(World), query).

-spec world(query()) -> any().
world({_E, _C, World}) ->
    World.

% This can potentially create an unbounded number of atoms! Careful!
-spec to_map(entity()) -> map().
to_map({EntityID, Components}) ->
    % Create the component map
    EMap = maps:from_list(Components),
    % Add the ID
    EMap#{id => EntityID}.

-spec get(term(), [component()]) -> term().
get(Component, ComponentList) ->
    get(Component, ComponentList, false).

-spec get(term(), [component()], term()) -> term().
get(Component, ComponentList, Default) ->
    case lists:keyfind(Component, 1, ComponentList) of
        {_Component, Data} ->
            Data;
        false ->
            Default
    end.

-spec take(term(), [component()]) -> term().
take(Component, ComponentList) ->
    take(Component, ComponentList, false).

-spec take(term(), [component()], term()) -> {term(), [term()]}.
take(Component, ComponentList, Default) ->
    case lists:keytake(Component, 1, ComponentList) of
        {value, {_Component, Data}, Rest} ->
            {Data, Rest};
        false ->
            Default
    end.

-spec try_component(term(), id(), query()) -> [term()] | false.
try_component(ComponentName, EntityID, Query) ->
    {ETable, CTable, _Name} = Query,
    case ets:match_object(CTable, {ComponentName, EntityID}) of
        [] ->
            false;
        _Match ->
            % It exists in the component table, so return the Entity data back
            % to the caller
            [{EntityID, Data}] = ets:lookup(ETable, EntityID),
            Data
    end.

-spec match_component(term(), query()) -> [entity()].
match_component(ComponentName, Query) ->
    % From the component bag table, get all matches
    {ETable, CTable, _Name} = Query,
    Matches = ets:lookup(CTable, ComponentName),
    % Use the entity IDs from the lookup in the component table to generate a
    % list of IDs for which to return data to the caller
    lists:flatten([ets:lookup(ETable, EntityID) || {_, EntityID} <- Matches]).

-spec match_components([term()], query()) -> [entity()].
match_components(List, Query) ->
    % Multi-match. Try to match several components and return the common
    % elements. Use sets v2 introduced in OTP 24
    Sets = [
        sets:from_list(match_component(X, Query), [{version, 2}])
     || X <- List
    ],
    sets:to_list(sets:intersection(Sets)).

-spec foreach_component(fun(), term(), query()) -> ok.
foreach_component(Fun, Component, Query) ->
    Entities = ow_ecs:match_component(Component, Query),
    F =
        fun({ID, EntityComponents}) ->
            Values = get(Component, EntityComponents),
            Fun(ID, Values)
        end,
    lists:foreach(F, Entities).

-spec new_entity(id(), query()) -> ok.
new_entity(EntityID, Query) ->
    {E, _C, _W} = Query,
    case ets:lookup(E, EntityID) of
        [] ->
            % ok, add 'em
            ets:insert(E, {EntityID, []});
        _Entity ->
            ok
    end.

-spec rm_entity(id(), query()) -> ok.
rm_entity(EntityID, Query) ->
    {E, C, _W} = Query,
    case ets:lookup(E, EntityID) of
        [] ->
            % ok, nothing to do
            ok;
        [{EntityID, Components}] ->
            % Remove the entity from the entity table
            ets:delete(E, EntityID),
            % Delete all instances of it from the component table as well
            [ets:delete_object(C, {N, EntityID}) || {N, _} <- Components],
            ok
    end.

-spec entity(id(), query()) -> {id(), [term()]}.
entity(EntityID, Query) ->
    {E, _C, _W} = Query,
    case ets:lookup(E, EntityID) of
        [] -> false;
        [Entity] -> Entity
    end.

-spec entities(query()) -> [{id(), [term()]}].
entities(Query) ->
    {E, _C, _W} = Query,
    ets:match_object(E, {'$0', '$1'}).

-spec add_component(term(), term(), id(), query()) -> true.
add_component(ComponentName, ComponentData, EntityID, Query) ->
    {E, C, _W} = Query,
    % On the entity table, we want to get the entity by key and insert a new
    % version with the data
    Components =
        case ets:lookup(E, EntityID) of
            [] ->
                % No components
                [{ComponentName, ComponentData}];
            [{EntityID, ComponentList}] ->
                % Check if the component already exists
                case lists:keytake(ComponentName, 1, ComponentList) of
                    {value, _Tuple, ComponentList2} ->
                        % Throw away the old data and add the new data
                        [{ComponentName, ComponentData} | ComponentList2];
                    false ->
                        [{ComponentName, ComponentData} | ComponentList]
                end
        end,
    % Insert the new entity and component list
    ets:insert(E, {EntityID, Components}),
    % Insert the entity EntityID into the component table
    ets:insert(C, {ComponentName, EntityID}).

-spec add_components([{term(), term()}], id(), query()) -> ok.
add_components(Components, EntityID, Query) ->
    F =
        fun({Component, Data}) ->
            add_component(Component, Data, EntityID, Query)
        end,
    lists:foreach(F, Components).

-spec del_component(term(), id(), query()) -> true.
del_component(ComponentName, EntityID, Query) ->
    {E, C, _W} = Query,
    % Remove the data from the entity
    case ets:lookup_element(E, EntityID, 2) of
        [] ->
            ok;
        ComponentList ->
            % Delete the key-value identified by ComponentName
            ComponentList1 = lists:keydelete(
                ComponentName, 1, ComponentList
            ),
            % Update the entity table
            ets:insert(E, {EntityID, ComponentList1})
    end,
    % Remove the data from the component bag
    ets:delete_object(C, {ComponentName, EntityID}).

-spec del_components([term()], id(), query()) -> ok.
del_components(Components, EntityID, Query) ->
    F =
        fun(Component) ->
            del_component(Component, EntityID, Query)
        end,
    lists:foreach(F, Components).

-spec add_system(system(), any() | query()) -> ok.
add_system(System, {_E, _C, World}) ->
    add_system(System, World);
add_system(System, World) ->
    add_system(System, 100, World).

-spec add_system(system(), integer(), any() | query()) -> ok.
add_system(System, Priority, {_E, _C, World}) ->
    add_system(System, Priority, World);
add_system(System, Priority, World) ->
    gen_server:call(?SERVER(World), {add_system, System, Priority}).

-spec del_system(system(), any() | query()) -> ok.
del_system(System, {_E, _C, World}) ->
    del_system(System, World);
del_system(System, World) ->
    gen_server:call(?SERVER(World), {del_system, System}).

-spec proc(any(), any()) -> ok.
proc({_E, _C, World}, Data) ->
    proc(World, Data);
proc(World, Data) ->
    gen_server:call(?SERVER(World), {proc, Data}).

-spec proc(any()) -> ok.
proc({_E, _C, World}) ->
    proc(World);
proc(World) ->
    gen_server:call(?SERVER(World), {proc, []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([WorldName]) ->
    World = #world{
        name = WorldName,
        systems = [],
        entities = ets:new(entities, [set, public]),
        components = ets:new(components, [bag, public])
    },
    logger:notice("Started ECS server: ~p", [WorldName]),
    logger:debug("Entity table ref: ~p", [World#world.entities]),
    logger:debug("Component table ref: ~p", [World#world.components]),
    {ok, World}.

handle_call({proc, Data}, _From, State) ->
    #world{systems = S, entities = E, components = C, name = Name} = State,
    % Process all systems in order
    Fun = fun({_Prio, Sys}) ->
        Query = {E, C, Name},
        case Sys of
            {M, F, 1} ->
                erlang:apply(M, F, [Query]);
            {M, F, 2} ->
                erlang:apply(M, F, [Query, Data]);
            Fun ->
                Fun(Query)
        end
    end,
    lists:foreach(Fun, S),
    {reply, ok, State};
handle_call(query, _From, State) ->
    #world{entities = E, components = C, name = Name} = State,
    Query = {E, C, Name},
    {reply, Query, State};
handle_call({add_system, Callback, Prio}, _From, State) ->
    #world{systems = S} = State,
    S0 =
        case lists:keytake(Callback, 2, S) of
            false ->
                S;
            {value, _Tuple, SRest} ->
                % Replace the current value instead
                SRest
        end,
    S1 = lists:keysort(1, [{Prio, Callback} | S0]),
    Reply = {ok, Prio},
    {reply, Reply, State#world{systems = S1}};
handle_call({del_system, Callback}, _From, State = #world{systems = S}) ->
    S1 = lists:keydelete(Callback, 2, S),
    {reply, ok, State#world{systems = S1}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
