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
    proc/1,
    proc/2,
    to_map/1,
    get/3,
    get/2,
    take/3,
    take/2
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

-type world() :: #world{}.
-export_type([world/0]).
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

-spec try_component(term(), id(), world()) -> [term()] | false.
try_component(ComponentName, EntityID, World) ->
    gen_server:call(
        ?SERVER(World), {try_component, ComponentName, EntityID}
    ).

-spec match_component(term(), world()) -> [entity()].
match_component(ComponentName, World) ->
    gen_server:call(?SERVER(World), {match_component, ComponentName}).

-spec match_components([term()], world()) -> [entity()].
match_components(List, World) ->
    % Multi-match. Try to match several components and return the common
    % elements. Use sets v2 introduced in OTP 24
    Sets = [
        sets:from_list(match_component(X, World), [{version, 2}])
     || X <- List
    ],
    sets:to_list(sets:intersection(Sets)).

-spec foreach_component(fun(), term(), world()) -> ok.
foreach_component(Fun, Component, World) ->
    Entities = match_component(Component, World),
    F =
        fun({ID, EntityComponents}) ->
            Values = get(Component, EntityComponents),
            Fun(ID, Values)
        end,
    lists:foreach(F, Entities).

-spec new_entity(id(), world()) -> ok.
new_entity(EntityID, World) ->
    gen_server:call(?SERVER(World), {new_entity, EntityID}).

-spec rm_entity(id(), world()) -> ok.
rm_entity(EntityID, World) ->
    gen_server:call(?SERVER(World), {rm_entity, EntityID}).

-spec entity(id(), world()) -> [term()] | false.
entity(EntityID, World) ->
    gen_server:call(?SERVER(World), {entity, EntityID}).

-spec entities(world()) -> [{id(), list()}].
entities(World) ->
    gen_server:call(?SERVER(World), entities).

-spec add_component(term(), term(), id(), world()) -> true.
add_component(Name, Data, EntityID, World) ->
    gen_server:call(?SERVER(World), {add_component, Name, Data, EntityID}).

% TODO: ok -> true?
-spec add_components([{term(), term()}], id(), world()) -> ok.
add_components(Components, EntityID, World) ->
    F =
        fun({Component, Data}) ->
            add_component(Component, Data, EntityID, World)
        end,
    lists:foreach(F, Components).

-spec del_component(term(), id(), world()) -> true.
del_component(Name, EntityID, World) ->
    gen_server:call(?SERVER(World), {del_component, Name, EntityID}).

% TODO: ok -> true?
-spec del_components([term()], id(), world()) -> ok.
del_components(Components, EntityID, World) ->
    F =
        fun(Component) ->
            del_component(Component, EntityID, World)
        end,
    lists:foreach(F, Components).

-spec add_system(system(), world()) -> ok.
add_system(System, World) ->
    add_system(System, 100, World).

-spec add_system(system(), integer(), world()) -> ok.
add_system(System, Priority, World) ->
    gen_server:call(?SERVER(World), {add_system, System, Priority}).

-spec del_system(system(), world()) -> ok.
del_system(System, World) ->
    gen_server:call(?SERVER(World), {del_system, System}).

-spec proc(world()) -> any().
proc(World) ->
    proc(World, []).

-spec proc(world(), any()) -> [any()].
proc(World, Data) ->
    Systems = gen_server:call(?SERVER(World), systems),
    Fun = fun({_Prio, Sys}, Acc) ->
        Result =
            case Sys of
                {M, F, 1} ->
                    erlang:apply(M, F, [World]);
                {M, F, 2} ->
                    erlang:apply(M, F, [World, Data]);
                Fun2 ->
                    Fun2(World, Data)
            end,
        [Result | Acc]
    end,
    lists:foldl(Fun, [], Systems).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server callbacks
%%%%%%%/%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

handle_call(
    {add_component, ComponentName, ComponentData, EntityID}, _From, State
) ->
    #world{entities = E, components = C} = State,
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
    ets:insert(C, {ComponentName, EntityID}),
    {reply, ok, State};
handle_call({del_component, ComponentName, EntityID}, _From, State) ->
    #world{entities = E, components = C} = State,
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
    ets:delete_object(C, {ComponentName, EntityID}),
    {reply, true, State};
handle_call({try_component, ComponentName, EntityID}, _From, State) ->
    #world{entities = E, components = C} = State,
    Resp =
        case ets:match_object(C, {ComponentName, EntityID}) of
            [] ->
                false;
            _Match ->
                % It exists in the component table, so return the Entity data
                % back to the caller
                [{EntityID, Data}] = ets:lookup(E, EntityID),
                Data
        end,
    {reply, Resp, State};
handle_call({match_component, ComponentName}, _From, State) ->
    % From the component bag table, get all matches
    #world{entities = E, components = C} = State,
    Matches = ets:lookup(C, ComponentName),
    % Use the entity IDs from the lookup in the component table to
    % generate a list of IDs for which to return data to the caller
    IDs = [ets:lookup(E, EntityID) || {_, EntityID} <- Matches],
    Resp = lists:flatten(IDs),
    {reply, Resp, State};
handle_call({new_entity, EntityID}, _From, State) ->
    #world{entities = E} = State,
    Resp =
        case ets:lookup(E, EntityID) of
            [] ->
                % ok, add 'em
                ets:insert(E, {EntityID, []});
            _Entity ->
                ok
        end,
    {reply, Resp, State};
handle_call({rm_entity, EntityID}, _From, State) ->
    #world{entities = E, components = C} = State,
    Resp =
        case ets:lookup(E, EntityID) of
            [] ->
                % ok, nothing to do
                ok;
            [{EntityID, Components}] ->
                % Remove the entity from the entity table
                ets:delete(E, EntityID),
                % Delete all instances of it from the component table as well
                [
                    ets:delete_object(C, {N, EntityID})
                 || {N, _} <- Components
                ],
                ok
        end,
    {reply, Resp, State};
handle_call({entity, EntityID}, _From, State) ->
    #world{entities = E} = State,
    Resp =
        case ets:lookup(E, EntityID) of
            [] ->
                false;
            [Entity] ->
                {_ID, Components} = Entity,
                Components
        end,
    {reply, Resp, State};
handle_call(entities, _From, State) ->
    #world{entities = E} = State,
    ets:match_object(E, {'$0', '$1'});
handle_call(systems, _From, State) ->
    #world{systems = S} = State,
    {reply, S, State};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
