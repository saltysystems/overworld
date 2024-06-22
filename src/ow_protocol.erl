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
    register/1,
    apps/0,
    app_names/0,
    prefix/1,
    rpc/2,
    rpcs/1,
    route/2,
    router/1
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
-spec start() -> gen_server:start_ret().
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-------------------------------------------------------------------------
%% @doc Stop the gen_server
%% @end
%%-------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%%-------------------------------------------------------------------------
%%% @doc Register a new game application with Overworld. This is required
%%%      for Overworld to generate a downloadable zips of protobuf, etc
%%%      files.  Returns {ok, NewState} on success and {{error, Reason},
%%%      State}} if the registration fails.
%%% @end
%%%-------------------------------------------------------------------------
-spec register(map()) ->
    {reply, ok | {error, atom()}, map()}.
register(App) ->
    gen_server:call(?MODULE, {register, App}).

%%-------------------------------------------------------------------------
%% @doc Get all apps registered with the server, including prefix and
%%      decoder module definition
%% @end
%%-------------------------------------------------------------------------
-spec apps() -> [{non_neg_integer(), map()}].
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
    term().
route(<<Prefix:16, Msg/binary>>, Session) ->
    % Get the decoder M/F for a given Overworld application
    case ow_protocol:router(Prefix) of
        false ->
            logger:notice("No router for prefix: 0x~.16b", [Prefix]),
            logger:notice("The rest of the message: ~p", [Msg]);
        Mod ->
            % Assume this app implements the ow_router behaviour
            erlang:apply(Mod, decode, [Msg, Session])
    end.

%%-------------------------------------------------------------------------
%% @doc Return the module and decoder function for a given prefix
%% @end
%%-------------------------------------------------------------------------
-spec router(integer()) -> atom().
router(Prefix) ->
    gen_server:call(?MODULE, {router, Prefix}).

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

handle_call({register, App}, _From, St0) ->
    % Fold over the list of modules to register them
    Modules = maps:get(modules, App, []),
    F = fun(Module, State) ->
        reg_rpc(Module, State)
    end,
    St1 = lists:foldl(F, St0, Modules),
    % Get the currently registered apps
    #{apps := Apps0} = St1,
    Apps1 =
        case maps:get(prefix, App, undefined) of
            undefined ->
                reg_app(App, Apps0);
            Prefix ->
                reg_app(Prefix, App, Apps0)
        end,
    {reply, ok, St1#{apps := Apps1}};
handle_call(apps, _From, #{apps := Apps} = St0) ->
    {reply, Apps, St0};
handle_call({prefix, PrefixName}, _From, #{apps := Apps} = St0) ->
    %[Prefix] = [P || {P,App} <- Apps, App == PrefixName],
    [Prefix] = [P || {P, #{app := App}} <- Apps, App == PrefixName],
    {reply, Prefix, St0};
handle_call(app_names, _From, #{apps := Apps} = St0) ->
    Names = [App || {_P, #{app := App}} <- Apps],
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
handle_call({router, Prefix}, _From, #{apps := Apps} = St0) ->
    Reply =
        case orddict:is_key(Prefix, Apps) of
            true ->
                App = orddict:fetch(Prefix, Apps),
                #{router := Router} = App,
                Router;
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

-spec reg_app(pos_integer(), map(), list()) -> list().
reg_app(Prefix, App, AppList) ->
    % Note that this will bump the next available slot up.
    orddict:store(Prefix, App, AppList).
-spec reg_app(map(), list()) -> list().
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
    Test1 = #{app => test1, router => test1_msg, modules => []},
    Apps1 = reg_app(Test1, Apps0),
    ?assertEqual(true, orddict:is_key(0, Apps1)),
    % Try adding another
    Test2 = #{app => test2, router => test2_msg, modules => []},
    Apps2 = reg_app(Test2, Apps1),
    ?assertEqual(true, orddict:is_key(1, Apps2)),
    ok.

-spec reg_app_prefix_test() -> ok.
reg_app_prefix_test() ->
    Apps0 = orddict:new(),
    Test1 = #{app => test1, router => test1_msg, modules => []},
    Apps1 = reg_app(10, Test1, Apps0),
    ?assertEqual(true, orddict:is_key(10, Apps1)),
    % Try adding another to see if it increments properly
    Test2 = #{app => test2, router => test2_msg, modules => []},
    Apps2 = reg_app(Test2, Apps1),
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
        maps:merge(ow_router:defaults(), Val)
    end,
    maps:map(F, PropMap).

-spec setup_propmap_tests() -> map().
setup_propmap_tests() ->
    PropList = [
        foo,
        {bar, {qos, reliable}},
        {baz,
            {
                encoder,
                #{app => test, lib => test_pb, interface => test_msg}
            }},
        {bop,
            {
                encoder,
                #{lib => special_pb}
            }}
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
    % Try to guess the encoder module based on convention
    ModuleString = erlang:atom_to_list(Module),
    [Prefix | _Rest] = string:split(ModuleString, "_", trailing),
    App = erlang:list_to_atom(Prefix),
    % Make the best guess for lib and interface modules
    EncoderLib = erlang:list_to_atom(Prefix ++ "_pb"),
    EncoderInterface = erlang:list_to_atom(Prefix ++ "_msg"),
    DefaultMap = #{
        app => App,
        lib => EncoderLib,
        interface => EncoderInterface
    },
    E =
        case proplists:get_value(rpc_encoder, Attributes) of
            undefined ->
                DefaultMap;
            [Encoder] ->
                Encoder
        end,
    F = fun
        (_Key, #{encoder := Existing} = Val) ->
            % Existing encoder found, merge with defaults to fill in any gaps
            Merged = maps:merge(DefaultMap, Existing),
            Val#{encoder => Merged};
        (_Key, Val) ->
            Val#{encoder => E}
    end,
    maps:map(F, PropMap).

-spec inject_encoder_test() -> ok.
inject_encoder_test() ->
    Map = setup_propmap_tests(),
    % ow_app doesn't have anything defined so it ought to generate defaults
    EncMap = inject_encoder('ow_app', Map),
    ExpectedFoo = #{
        encoder => #{
            app => ow,
            lib => overworld_pb,
            interface => ow_msg
        }
    },
    ?assertEqual(ExpectedFoo, maps:get(foo, EncMap)),
    ExpectedBaz = #{
        encoder => #{
            app => test,
            lib => test_pb,
            interface => test_msg
        }
    },
    ?assertEqual(ExpectedBaz, maps:get(baz, EncMap)),
    ExpectedBop = #{
        encoder => #{
            app => ow,
            lib => special_pb,
            interface => ow_msg
        }
    },
    ?assertEqual(ExpectedBop, maps:get(bop, EncMap)),
    ok.
