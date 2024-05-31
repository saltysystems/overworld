-module(ow_ecs2).

%% API
-export([start/0, stop/1]).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Types and Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(world, {
    systems = [] :: [{term(), system()}],
    entities :: ets:tid(),
    components :: ets:tid()
}).

-opaque world() :: #world{}.
-export_type([world/0]).
-type component() :: {term(), term()}.
-type entity() :: {term(), [component()]}.
-export_type([entity/0]).
-type system() :: {mfa() | fun()}.
-type id() :: integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start() -> world().
start() ->
    #world{
        systems = [],
        entities = ets:new(entities, [set, public]),
        components = ets:new(components, [bag, public])
    }.

-spec stop(world()) -> ok.
stop(#world{entities = ETable, components = CTable}) ->
    ets:delete(ETable),
    ets:delete(CTable),
    ok.

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

-spec take(term(), [component()]) -> term() | false.
take(Component, ComponentList) ->
    take(Component, ComponentList, false).

-spec take(term(), [component()], term()) -> {term(), [term()]} | term().
take(Component, ComponentList, Default) ->
    case lists:keytake(Component, 1, ComponentList) of
        {value, {_Component, Data}, Rest} ->
            {Data, Rest};
        false ->
            Default
    end.

-spec new_entity(id(), world()) -> ok.
new_entity(EntityID, World) ->
    #world{entities = E} = World,
    case ets:lookup(E, EntityID) of
        [] ->
            % ok, add 'em
            ets:insert(E, {EntityID, []});
        _Entity ->
            ok
    end.

-spec rm_entity(id(), world()) -> ok.
rm_entity(EntityID, World) ->
    #world{entities = E, components = C} = World,
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
    end.

-spec entity(id(), world()) -> {id(), [term()]} | false.
entity(EntityID, World) ->
    #world{entities = E} = World,
    case ets:lookup(E, EntityID) of
        [] ->
            false;
        [Entity] ->
            Entity
    end.

-spec entities(world()) -> [{id(), [term()]}].
entities(World) ->
    #world{entities = E} = World,
    ets:match_object(E, {'$0', '$1'}).

-spec try_component(term(), id(), world()) -> {ok, [term()]} | false.
try_component(ComponentName, EntityID, World) ->
    #world{entities = E, components = C} = World,
    case ets:match_object(C, {ComponentName, EntityID}) of
        [] ->
            false;
        _Match ->
            % It exists in the component table, so return the Entity data
            % back to the caller
            [{EntityID, Data}] = ets:lookup(E, EntityID),
            {ok, Data}
    end.

-spec match_component(term(), world()) -> [entity()].
match_component(ComponentName, World) ->
    % From the component bag table, get all matches
    #world{entities = E, components = C} = World,
    Matches = ets:lookup(C, ComponentName),
    % Use the entity IDs from the lookup in the component table to
    % generate a list of IDs for which to return data to the caller
    IDs = [ets:lookup(E, EntityID) || {_, EntityID} <- Matches],
    lists:usort(lists:flatten(IDs)).

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

-spec add_component(term(), term(), id(), world()) -> ok.
add_component(Name, Data, EntityID, World) ->
    #world{entities = E, components = C} = World,
    % On the entity table, we want to get the entity by key and insert a new
    % version with the data
    Components =
        case ets:lookup(E, EntityID) of
            [] ->
                % No components
                [{Name, Data}];
            % BEHAVIOUR CHANGE
            [{EntityID, ComponentList}] ->
                case lists:keytake(Name, 1, ComponentList) of
                    {value, _Tuple, ComponentList2} ->
                        % Throw away the old data and add the new data
                        [{Name, Data} | ComponentList2];
                    false ->
                        [{Name, Data} | ComponentList]
                end
        end,
    % Insert the new entity and component list
    ets:insert(E, {EntityID, Components}),
    % Insert the entity EntityID into the component table
    ets:insert(C, {Name, EntityID}),
    ok.

-spec add_components([{term(), term()}], id(), world()) -> ok.
add_components(Components, EntityID, World) ->
    F =
        fun({Component, Data}) ->
            add_component(Component, Data, EntityID, World)
        end,
    lists:foreach(F, Components),
    ok.

-spec del_component(term(), id(), world()) -> ok.
del_component(Name, EntityID, World) ->
    #world{entities = E, components = C} = World,
    % Remove the data from the entity
    case ets:lookup_element(E, EntityID, 2) of
        [] ->
            ok;
        ComponentList ->
            % Delete the key-value identified by ComponentName
            ComponentList1 = lists:keydelete(
                Name, 1, ComponentList
            ),
            % Update the entity table
            ets:insert(E, {EntityID, ComponentList1})
    end,
    % Remove the data from the component bag
    ets:delete_object(C, {Name, EntityID}),
    ok.

-spec del_components([term()], id(), world()) -> ok.
del_components(Components, EntityID, World) ->
    F =
        fun(Component) ->
            del_component(Component, EntityID, World)
        end,
    lists:foreach(F, Components),
    ok.

-spec add_system(system(), world()) -> {ok, world()}.
add_system(Callback, World) ->
    add_system(Callback, 100, World).

-spec add_system(system(), integer(), world()) -> {ok, world()}.
add_system(Callback, Priority, World) ->
    #world{systems = S} = World,
    S0 =
        case lists:keytake(Callback, 2, S) of
            false ->
                S;
            {value, _Tuple, SRest} ->
                % Replace the current value instead
                SRest
        end,
    S1 = lists:keysort(1, [{Priority, Callback} | S0]),
    {ok, World#world{systems = S1}}.

-spec del_system(system(), world()) -> {ok, world()}.
del_system(Callback, World) ->
    #world{systems = S} = World,
    S1 = lists:keydelete(Callback, 2, S),
    {ok, World#world{systems = S1}}.

-spec proc(world()) -> any().
proc(World) ->
    proc([], World).

-spec proc(any(), world()) -> [any()].
proc(Data, World) ->
    #world{systems = Systems} = World,
    Fun = fun({_Prio, Sys}, Acc) ->
        Result =
            case Sys of
                {M, F, 1} ->
                    erlang:apply(M, F, [World]);
                {M, F, 2} ->
                    erlang:apply(M, F, [Data, World]);
                Fun2 ->
                    Fun2(Data, World)
            end,
        [{Sys, Result} | Acc]
    end,
    lists:foldl(Fun, [], Systems).
