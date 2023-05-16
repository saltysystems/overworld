%%=========================================================================
%% Overworld Protocol
%%
%% This module handles:
%%  - Registering/deregistering new opcodes for message types
%%  - Routing messages to appropriate modules to deserialization
%%
%%=========================================================================
-module(ow_protocol).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

% public API
-export([
    start/0,
    stop/0,
    register_rpc/1,
    register_app/1,
    register_app/2,
    apps/0,
    app_names/0,
    prefix/1,
    rpc/2,
    rpcs/1,
    route/2,
    handler/1,
    response/1,
    response/2
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%=========================================================================
%% API
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc Start the gen_server
%% @end
%%-------------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------------
%% @doc Stop the gen_server
%% @end
%%-------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%-------------------------------------------------------------------------
%% @doc Register a new Erlang application with Overworld. This is required
%%      for Overworld to generate a downloadable zips of protobuf, etc
%%      files.  Returns {ok, NewState} on success and {{error, Reason},
%%      State}} if the registration fails.
%% @end
%%-------------------------------------------------------------------------
-spec register_app(pos_integer(), {atom(), {atom(), atom()}}) ->
    {reply, ok | {error, atom()}, map()}.
register_app(Prefix, Application) ->
    gen_server:call(?MODULE, {register_app, Prefix, Application}).

-spec register_app({atom(), {atom(), atom()}}) ->
    {reply, ok | {error, atom()}, map()}.
register_app(Application) ->
    gen_server:call(?MODULE, {register_app, Application}).

%%-------------------------------------------------------------------------
%% @doc Register a new opcode and associated callback function for Module.
%%      Returns {ok, NewState} on success and {{error, Reason}, State}} if
%%      the registration fails.
%% @end
%%-------------------------------------------------------------------------
-spec register_rpc(atom()) -> {reply, ok | {error, atom()}, map()}.
register_rpc(Module) ->
    logger:info("Registering callbacks for : ~p~n", [Module]),
    gen_server:call(?MODULE, {register_rpc, Module}).

%%-------------------------------------------------------------------------
%% @doc Encode a general response
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error) -> binary().
response(ok) ->
    ow_msg:encode(#{status => 'OK'}, gen_response);
response(error) ->
    ow_msg:encode(#{status => 'ERROR'}, gen_response).

%%-------------------------------------------------------------------------
%% @doc Encode a general response with reasoning
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error, string()) -> binary().
response(ok, Msg) ->
    ow_msg:encode(
        #{
            status => 'OK',
            msg => Msg
        },
        gen_response
    );
response(error, Msg) ->
    ow_msg:encode(
        #{
            status => 'ERROR',
            msg => Msg
        },
        gen_response
    ).

%%-------------------------------------------------------------------------
%% @doc Get all apps registered with the server, including prefix and 
%%      decoder module definition
%% @end
%%-------------------------------------------------------------------------
-spec apps() -> [{non_neg_integer(), {atom(), {atom(), atom()}}}].
apps() ->
    gen_server:call(?MODULE, apps).

%%-------------------------------------------------------------------------
%% @doc Get a list of Overworld applications registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec app_names() -> [atom()].
app_names() ->
    gen_server:call(?MODULE, app_names).

%%-------------------------------------------------------------------------
%% @doc Get the base-10 prefix for a particular application
%% @end
%%-------------------------------------------------------------------------
-spec prefix(atom()) -> non_neg_integer().
prefix(Name) -> 
    gen_server:call(?MODULE, {prefix, Name}).

%%-------------------------------------------------------------------------
%% @doc Get a list of all RPCs registered with the server (keys only).
%%      Type options are: all, client, server
%% @end
%%-------------------------------------------------------------------------
-spec rpcs(all | client | server) -> list().
rpcs(Type) ->
    gen_server:call(?MODULE, {rpcs, Type}).

%%-------------------------------------------------------------------------
%% @doc Return the full map for a particular RPC
%%      Type options are: client, server
%% @end
%%-------------------------------------------------------------------------
-spec rpc(atom(), client | server) -> map().
rpc(RPC, Type) ->
    gen_server:call(?MODULE, {rpc, RPC, Type}).

%%-------------------------------------------------------------------------
%% @doc Route a message to the appropriate Overworld application based on
%%      application prefix
%% @end
%%-------------------------------------------------------------------------
-spec route(<<_:16, _:_*8>>, ow_session:session()) ->
    ow_session:net_msg().
route(<<Prefix:16, Msg/binary>>, Session) ->
    % Get the decoder M/F for a given Overworld application
    case ow_protocol:handler(Prefix) of
        false ->
            logger:notice("No handler for prefix: 0x~.16b", [Prefix]),
            logger:notice("The rest of the message: ~p", [Msg]);
        {Mod, Fun} -> 
            % Now call
            erlang:apply(Mod, Fun, [Msg, Session])
    end.

%%-------------------------------------------------------------------------
%% @doc Return the module and decoder function for a given prefix
%% @end
%%-------------------------------------------------------------------------
handler(Prefix) ->
    gen_server:call(?MODULE, {handler, Prefix}).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init([]) ->
    % Initialize the state
    St0 = #{
        % client RPCs
        c_rpc => #{},
        % server RPCs
        s_rpc => #{},
        apps => []
    },
    {ok, St0}.

handle_call({register_rpc, Module}, _From, St0) ->
    % Get list of RPCs to register for Module
    %Ops0 = maps:get(ops, St0),
    St1 = reg_rpc(Module, St0),
    {reply, ok, St1};
handle_call(
    {register_app, Prefix, App}, _F, #{apps := Apps} = St0
) ->
    % Get list of opcodes to register for Module
    Apps1 = reg_app(Prefix, App, Apps),
    {reply, ok, St0#{apps := Apps1}};
handle_call({register_app, App}, _From, #{apps := Apps} = St0) ->
    % Get list of opcodes to register for Module
    Apps1 = reg_app(App, Apps),
    {reply, ok, St0#{apps := Apps1}};
handle_call(apps, _From, St0) ->
    Apps = maps:get(apps, St0),
    {reply, Apps, St0};
handle_call({prefix, PrefixName}, _From, St0) ->
    Apps = maps:get(apps, St0),
    [Prefix] = [ P || {P, {App, _ModFun}} <- Apps, App == PrefixName],
    {reply, Prefix, St0};
handle_call(app_names, _From, St0) ->
    Apps = maps:get(apps, St0),
    Names = [ App || {_P, {App, _ModFun}} <- Apps],
    {reply, Names, St0};
handle_call({rpcs, all}, _From, #{c_rpc := C, s_rpc := S} = St0) ->
    Reply = maps:keys(C) ++ maps:keys(S),
    {reply, Reply, St0};
handle_call({rpcs, client}, _From, #{c_rpc := C} = St0) ->
    Reply = maps:keys(C),
    {reply, Reply, St0};
handle_call({rpcs, server}, _From, #{s_rpc := S} = St0) ->
    Reply = maps:keys(S),
    {reply, Reply, St0};
handle_call({rpc, RPC, client}, _From, #{c_rpc := C} = St0) ->
    Reply = maps:get(RPC, C),
    {reply, Reply, St0};
handle_call({rpc, RPC, server}, _From, #{s_rpc := S} = St0) ->
    Reply = maps:get(RPC, S),
    {reply, Reply, St0};
handle_call({handler, Prefix}, _From, #{apps := Apps} = St0) ->
    Reply = 
        case orddict:is_key(Prefix, Apps) of
            true -> 
                orddict:fetch(Prefix, Apps);
            false ->
                false
        end,
    {reply, Reply, St0}.

handle_cast(_Request, St0) ->
    {noreply, St0}.

handle_info(_Request, St0) ->
    {noreply, St0}.

terminate(_Reason, _St0) ->
    ok.

code_change(_OldVsn, St0, _Extra) ->
    {ok, St0}.

%%============================================================================
%% Internal functions
%%============================================================================
-spec reg_rpc(atom(), map()) -> map().
reg_rpc(Module, #{c_rpc := CRPC, s_rpc := SRPC} = St0) ->
    % Get module info for the module
    AllAttributes = erlang:apply(Module, module_info, [attributes]),
    F = fun(Attribute) ->
        case proplists:lookup(Attribute, AllAttributes) of
            none ->
                #{};
            {Attribute, Calls} ->
                M = deep_propmap(Calls),
                M2 = inject_defaults(M),
                M3 = inject_module(Module, M2),
                inject_encoder(Module, M3)
        end
    end,
    ClientMap = F(rpc_client),
    ServerMap = F(rpc_server),
    CRPC1 = maps:merge(ClientMap, CRPC),
    SRPC1 = maps:merge(ServerMap, SRPC),
    St0#{c_rpc => CRPC1, s_rpc => SRPC1}.

-spec reg_app(pos_integer(), {atom(), atom()}, list()) -> list().
reg_app(Prefix, App, AppList) ->
    % Note that this will bump the next available slot up.
    orddict:store(Prefix, App, AppList).
-spec reg_app({atom(), atom()}, list()) -> list().
reg_app(App, AppList) ->
    % Determine the next available prefix
    Next =
        case orddict:fetch_keys(AppList) of
            [] ->
                0;
            Keys ->
                Max = lists:max(Keys),
                Max + 1
        end,
    % The prefix is assumed to be 8-bits, weird stuff may happen beyond 255
    % apps
    orddict:store(Next, App, AppList).

-spec reg_app_test() -> ok.
reg_app_test() ->
    Apps0 = orddict:new(),
    Apps1 = reg_app({ow_test1, hello}, Apps0),
    ?assertEqual(true, orddict:is_key(0, Apps1)),
    % Try adding another
    Apps2 = reg_app({ow_test2, goodbye}, Apps1),
    ?assertEqual(true, orddict:is_key(1, Apps2)),
    ok.

-spec reg_app_prefix_test() -> ok.
reg_app_prefix_test() ->
    Apps0 = orddict:new(),
    Apps1 = reg_app(10, {ow_test1, foo}, Apps0),
    ?assertEqual(true, orddict:is_key(10, Apps1)),
    % Try adding another to see if it increments properly
    Apps2 = reg_app({ow_test2, bar}, Apps1),
    ?assertEqual(true, orddict:is_key(11, Apps2)),
    ok.

-spec deep_propmap(list()) -> map().
deep_propmap(PropList) ->
    deep_propmap(PropList, #{}).
deep_propmap([], Map) ->
    Map;
deep_propmap([H | T], Map) ->
    % Take the first item in the list and convert it to a map
    Map0 =
        case H of
            H when is_atom(H) ->
                #{H => #{}};
            {K, {K1, V1}} ->
                #{K => deep_propmap([{K1, V1}])};
            {K, V} ->
                #{K => V}
        end,
    Map1 = maps:merge(Map0, Map),
    deep_propmap(T, Map1).

-spec inject_module(atom(), map()) -> map().
inject_module(Module, PropMap) ->
    F = fun(_Key, Val) ->
        Val#{module => Module}
    end,
    maps:map(F, PropMap).

-spec inject_module_test() -> ok.
inject_module_test() ->
    Map = setup_propmap_tests(),
    InjMap = inject_module(ow_test, Map),
    ExpectedFoo = #{module => ow_test},
    ?assertEqual(ExpectedFoo, maps:get(foo, InjMap)),
    ExpectedBar = #{module => ow_test, qos => reliable},
    ?assertEqual(ExpectedBar, maps:get(bar, InjMap)),
    ok.

% TODO: Not clear this is the best place for it, but it's the most functional
%       place at the moment.
-spec inject_defaults(map()) -> map().
inject_defaults(PropMap) ->
    F = fun(_Key, Val) ->
        maps:merge(Val, ow_rpc:defaults())
    end,
    maps:map(F, PropMap).

-spec setup_propmap_tests() -> map().
setup_propmap_tests() ->
    PropList = [
        foo,
        {bar, {qos, reliable}},
        {baz, {encoder, test_pb}}
    ],
    deep_propmap(PropList).

-spec deep_propmap_test() -> ok.
deep_propmap_test() ->
    Map = setup_propmap_tests(),
    ?assertEqual(#{}, maps:get(foo, Map)),
    ?assertEqual(#{qos => reliable}, maps:get(bar, Map)),
    ok.

inject_encoder(Module, PropMap) ->
    Attributes = erlang:apply(Module, module_info, [attributes]),
    E =
        case proplists:lookup(rpc_encoder, Attributes) of
            none ->
                % Try to guess the encoder module based on convention
                ModuleString = erlang:atom_to_list(Module),
                [Prefix | _Rest] = string:split(ModuleString, "_", leading),
                % per GPB defaults
                EncoderGuess = Prefix ++ "_pb",
                % Try to convert to an exist atom or crash, we can't continue
                % without an encoder.
                erlang:list_to_existing_atom(EncoderGuess);
            {rpc_encoder, [Encoder]} ->
                Encoder
        end,
    F = fun
        (_Key, #{encoder := _Existing} = Val) ->
            % Existing encoder found, do nothing
            Val;
        (_Key, Val) ->
            Val#{encoder => E}
    end,
    maps:map(F, PropMap).

-spec inject_encoder_test() -> ok.
inject_encoder_test() ->
    Map = setup_propmap_tests(),
    EncMap = inject_encoder('overworld_pb', Map),
    ExpectedFoo = #{encoder => overworld_pb},
    ?assertEqual(ExpectedFoo, maps:get(foo, EncMap)),
    ExpectedBaz = #{encoder => test_pb},
    ?assertEqual(ExpectedBaz, maps:get(baz, EncMap)),
    ok.
