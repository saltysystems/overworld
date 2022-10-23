-module(ow_ecs).
-callback system_handler(world()) -> world().

%% api
-export([new/0, proc/1, match/3, match_all/2]).
-export([create_entity/1, delete_entity/2]).
-export([add_component/3, rm_component/3, maybe_component/3]).
-export([add_system/3, add_system/2, rm_system/2]).

% types
-type system() :: {integer(), mfa()}.
-type component_name() :: string().
-type component() :: {component_name(), any()}.
-type entity_id() :: integer().

-record(entity, {
    id = erlang:unique_integer() :: entity_id(),
    components = []
}).
-type entity() :: #entity{}.

-record(world, {
    systems = [] :: [system()],
    entities = [] :: [entity()]
}).
-opaque world() :: #world{}.
-export_type([world/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% World functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new() -> world().
new() ->
    #world{}.

-spec proc(world()) -> world().
proc(World) ->
    % Run the callback handlers for all systems in order of priority
    Fun = fun({_Prio, {M, F, 1}}, World0) ->
        % Apply the handler function and return the new world state
        World1 = erlang:apply(M, F, [World0]),
        World1
    end,
    lists:foldl(Fun, World, World#world.systems).

-spec match_all([component_name()], world()) -> [entity()].
match_all(ComponentNames, World = #world{entities = Entities}) ->
    % Pretty hacky
    {_, EntityIDs, _} = lists:unzip3(Entities),
    Fun =
        fun(EntityID, AccIn) ->
            case match(ComponentNames, EntityID, World) of
                false -> AccIn;
                E -> [E | AccIn]
            end
        end,
    lists:foldl(Fun, [], EntityIDs).

-spec match([component_name()], entity_id(), world()) -> entity() | false.
match(ComponentNames, EntityID, World) ->
    Fun =
        fun(C, Acc) ->
            M =
                case maybe_component(C, EntityID, World) of
                    false -> false;
                    _ -> true
                end,
            M and Acc
        end,
    case lists:foldl(Fun, true, ComponentNames) of
        false -> false;
        true -> get_entity(EntityID, World)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entity-specific functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_entity(world()) -> {entity_id(), world()}.
create_entity(World = #world{entities = Entities}) ->
    Entity = #entity{},
    World1 = World#world{entities = [Entity | Entities]},
    {Entity#entity.id, World1}.

-spec delete_entity(entity_id(), world()) -> {ok, world()}.
delete_entity(ID, World = #world{entities = Entities}) ->
    Entities1 = lists:keydelete(ID, #entity.id, Entities),
    {ok, World#world{entities = Entities1}}.

get_entity(ID, #world{entities = Entities}) ->
    lists:keyfind(ID, #entity.id, Entities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Component-specific functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add_component(component(), entity_id(), world()) ->
    {ok | already_exists, world()}.
add_component(Component, EntityID, World = #world{entities = Entities}) ->
    {value, Entity, Entities1} =
        lists:keytake(EntityID, #entity.id, Entities),
    Components = Entity#entity.components,
    {Result, Entity1} =
        case lists:member(Component, Components) of
            true ->
                {already_exists, Entity};
            false ->
                {ok, Entity#entity{components = [Component | Components]}}
        end,
    {Result, World#world{entities = [Entity1 | Entities1]}}.

-spec rm_component(component_name(), entity_id(), world()) -> {ok, world()}.
rm_component(ComponentName, EntityID, World = #world{entities = Entities}) ->
    {value, Entity, Entities1} =
        lists:keytake(EntityID, #entity.id, Entities),
    Components = Entity#entity.components,
    Components1 = lists:keydelete(ComponentName, 1, Components),
    Entity1 = Entity#entity{components = Components1},
    {ok, World#world{entities = [Entity1 | Entities1]}}.

-spec maybe_component(component_name(), entity_id(), world()) ->
    component() | false.
maybe_component(ComponentName, EntityID, World) ->
    % Get the entity
    case get_entity(EntityID, World) of
        false ->
            % can't find the entity, therefore can't find the component
            false;
        Entity ->
            lists:keyfind(ComponentName, 1, Entity#entity.components)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System-specific functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_system(System, World) ->
    add_system(System, 0, World).

add_system(System, Priority, World = #world{systems = Systems}) ->
    Systems1 = lists:keysort(1, [{Priority, System} | Systems]),
    World#world{systems = Systems1}.

rm_system(System, World = #world{systems = Systems}) ->
    Systems1 = lists:keydelete(2, System, Systems),
    World#world{systems = Systems1}.
