-module(ow_ecs).

-behaviour(gen_server).

-export([start/1, start_link/1]).

-define(SERVER(ComponentName),
    {via, gproc, {n, l, {?MODULE, ComponentName}}}
).

%% API
-export([
    rm_entity/2,
    add_component/4,
    rm_component/3,
    try_component/3,
    match_component/2,
    add_system/3,
    add_system/2,
    rm_system/2,
    world_name/1,
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
    name :: term(),
    systems = [] :: [{integer(), mfa() | fun()}],
    entities :: ets:tid(),
    components :: ets:tid()
}).
%-type world() :: #world{}.

-opaque query() :: {ets:tid(), ets:tid(), any()}.
-export_type([query/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Query, update, etc functions that operate against the ETS tables
-spec world_name(query()) -> any().
world_name({_, _, Name}) -> Name.

-spec try_component(term(), integer(), query()) -> [term()].
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

-spec match_component(term(), query()) -> [tuple()].
match_component(ComponentName, Query) ->
    % From the component bag table, get all matches
    {ETable, CTable, _Name} = Query,
    Matches = ets:lookup(CTable, ComponentName),
    % Use the entity IDs from the lookup in the component table to generate a
    % list of IDs for which to return data to the caller
    lists:flatten([ets:lookup(ETable, EntityID) || {_, EntityID} <- Matches]).

% Multi-match ... TODO ?
%match_component(CList, Query) when is_list(CList) ->
%    {ETable, CTable} = Query,
%    % http://erlang.org/pipermail/erlang-questions/2012-February/064214.html
%    % Return the subset of the bag that contains the entity IDs that match
%    PartialMatches = ets:select(CTable, [{{C,'$1'},[],['$1']} || C <- CList]),
%    % Need to filter out any entitiy IDs that don't appear at least N times,
%    % where N is len(CList). TODO: See if there's a faster way to do this with
%    % match expressions
%    [ ets:lookup(ETable, EntityID) || EntityID <- Matches ];
%match_component(Component, Query) ->
%    match_component([Component], Query).

% Functions that call out to the gen_server and somehow mutate state
% Not adding this one til proven useful
%add_entity(EntityID, World) ->
%    gen_server:cast(?SERVER(World), {add_entity, EntityID}).

rm_entity(EntityID, World) ->
    gen_server:cast(?SERVER(World), {rm_entity, EntityID}).

add_component(ComponentName, ComponentData, EntityID, World) ->
    gen_server:cast(
        ?SERVER(World),
        {add_component, ComponentName, ComponentData, EntityID}
    ).

rm_component(ComponentName, EntityID, World) ->
    gen_server:cast(
        ?SERVER(World), {rm_component, ComponentName, EntityID}
    ).

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

start(World) ->
    gen_server:start(?SERVER(World), ?MODULE, [World], []).
start_link(World) ->
    gen_server:start_link(?SERVER(World), ?MODULE, [World], []).

init([WorldName]) ->
    World = #world{
        name = WorldName,
        systems = [],
        entities = ets:new(entities, [set]),
        components = ets:new(components, [bag])
    },
    logger:notice("Started ECS server: ~p", [WorldName]),
    logger:debug("Entity table ref: ~p", [World#world.entities]),
    logger:debug("Component table ref: ~p", [World#world.components]),
    {ok, World}.

handle_call(proc, _From, State) ->
    #world{systems = S, entities = E, components = C, name = Name} = State,
    % Process all systems in order
    Fun = fun({_Prio, Sys}) ->
        Query = {E, C, Name},
        case Sys of
            {M, F, _A} ->
                erlang:apply(M, F, [Query]);
            Fun ->
                Fun(Query)
        end
    end,
    lists:foreach(Fun, S),
    {reply, ok, State}.

handle_cast({rm_entity, EntityID}, State) ->
    #world{entities = E, components = C} = State,
    case ets:lookup(E, EntityID) of
        [] ->
            % ok, nothing to do
            ok;
        [{EntityID, ComponentList}] ->
            % Remove the entity from the entity table
            ets:delete(E, EntityID),
            % Delete all instances of it from the component table as well
            [
                ets:delete_object(C, {CName, EntityID})
             || {CName, _} <- ComponentList
            ]
    end,
    {noreply, State};
handle_cast({add_component, ComponentName, ComponentData, EntityID}, State) ->
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
    {noreply, State};
handle_cast({rm_component, ComponentName, EntityID}, State) ->
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
    {noreply, State};
handle_cast({add_system, Callback, Prio}, State) ->
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
