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
    register_app/2,
    apps/0,
    app/1,
    rpcs/0,
    client_rpc/1,
    server_rpc/1,
    decode/2,
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

-include("db/ow_database.hrl").

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
-spec register_app(atom(), atom()) -> {reply, ok | {error, atom()}, map()}.
register_app(Application, Encoder) ->
    gen_server:call(?MODULE, {register_app, Application, Encoder}).

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
%% @doc Decode messages from clients and route them to an appropriate
%%      function
%% @end
%%-------------------------------------------------------------------------
-spec decode(binary(), ow_session:session()) ->
    ok
    | {ok, ow_session:session()}
    | {binary(), ow_session:session()}
    | {ok, ow_session:session(), ow_enet:qos()}
    | {binary(), ow_session:session(), ow_enet:qos()}.
decode(Message, Session) ->
    gen_server:call(?MODULE, {decode, Message, Session}).

%%-------------------------------------------------------------------------
%% @doc Encode a general response
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error) -> binary().
response(ok) ->
    overworld_pb:encode_msg(#{status => 'OK'}, gen_response);
response(error) ->
    overworld_pb:encode_msg(#{status => 'ERROR'}, gen_response).

%%-------------------------------------------------------------------------
%% @doc Encode a general response with reasoning
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error, string()) -> binary().
response(ok, Msg) ->
    overworld_pb:encode_msg(
        #{
            status => 'OK',
            msg => Msg
        },
        gen_response
    );
response(error, Msg) ->
    overworld_pb:encode_msg(
        #{
            status => 'ERROR',
            msg => Msg
        },
        gen_response
    ).

%%-------------------------------------------------------------------------
%% @doc Get a list of RPCs registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec rpcs() -> {list(), list()}.
rpcs() ->
    gen_server:call(?MODULE, rpcs).

%%-------------------------------------------------------------------------
%% @doc Get a list of Overworld applications registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec apps() -> [atom(), ...].
apps() ->
    gen_server:call(?MODULE, apps).

%%-------------------------------------------------------------------------
%% @doc Get information for a particular registered app
%% @end
%%-------------------------------------------------------------------------
-spec app(atom()) -> tuple().
app(App) ->
    gen_server:call(?MODULE, {app, App}).

%%-------------------------------------------------------------------------
%% @doc Return the info map for a particular opcode
%% @end
%%-------------------------------------------------------------------------
-spec client_rpc(atom()) -> map().
client_rpc(RPC) ->
    gen_server:call(?MODULE, {client_rpc, RPC}).

-spec server_rpc(atom()) -> map().
server_rpc(RPC) ->
    gen_server:call(?MODULE, {server_rpc, RPC}).

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
    % Get list of opcodes to register for Module
    %Ops0 = maps:get(ops, St0),
    St1 = reg_rpc(Module, St0),
    {reply, ok, St1};
handle_call(
    {register_app, Application, Encoder}, _From, #{apps := Apps} = St0
) ->
    % Get list of opcodes to register for Module
    Apps1 = reg_app(Application, Encoder, Apps),
    {reply, ok, St0#{apps := Apps1}};
handle_call({decode, Message, Session}, _From, St0) ->
    Reply = route(Message, Session, St0),
    {reply, Reply, St0};
handle_call(rpcs, _From, #{c_rpc := C, s_rpc := S} = St0) ->
    Reply = {{client_rpc, maps:keys(C)}, {server_rpc, maps:keys(S)}},
    {reply, Reply, St0};
handle_call(apps, _From, St0) ->
    Apps = maps:get(apps, St0),
    {reply, Apps, St0};
handle_call({app, App}, _From, #{apps := Apps} = St0) ->
    Reply = proplist:lookup(App, Apps),
    {reply, Reply, St0};
handle_call({client_rpc, RPC}, _From, #{c_rpc := C} = St0) ->
    Reply = maps:get(RPC, C),
    {reply, Reply, St0};
handle_call({server_rpc, RPC}, _From, #{s_rpc := S} = St0) ->
    Reply = maps:get(RPC, S),
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
-spec route(<<_:8, _:_*8>>, ow_session:session(), map()) ->
    ow_session:net_msg().
route(<<Prefix:8, Msg/binary>>, Session, #{apps := Apps}) ->
    % Message will be prefixed with an application identifier
    % Get the callback module and route the message as appropriate
    {Mod, Fun} = orddict:fetch(Prefix, Apps),
    erlang:apply(Mod, Fun, [Msg, Session]).

-spec reg_rpc(atom(), map()) -> map().
reg_rpc(Module, #{c_rpc := CRPC, s_rpc := SRPC} = St0) ->
    % Get module info for the module
    AllAttributes = erlang:apply(Module, module_info, [attributes]),
    F = fun(Attribute) ->
        case proplists:lookup(Attribute, AllAttributes) of
            none ->
                #{};
            {Attribute, Calls} ->
                deeper_propmap(Calls)
        end
    end,
    ClientMap = F(rpc_client),
    ServerMap = F(rpc_server),
    CRPC1 = maps:merge(ClientMap, CRPC),
    SRPC1 = maps:merge(ServerMap, SRPC),
    St0#{c_rpc => CRPC1, s_rpc => SRPC1}.

-spec reg_app(pos_integer(), {atom(), atom()}, atom(), list()) -> list().
reg_app(Prefix, App, Encoder, AppList) ->
    % Note that this will bump the next available slot up.
    orddict:store(Prefix, {App, Encoder}, AppList).
-spec reg_app({atom(), atom()}, atom(), list()) -> list().
reg_app(App, Encoder, AppList) ->
    % Determine the next available prefix
    Next =
        case orddict:fetch_keys(AppList) of
            [] ->
                0;
            Keys ->
                Max = lists:max(Keys),
                Max + 1
        end,
    % The prefix is assumed to be 8-bits, weird stuff may happen beyond 255 apps
    orddict:store(Next, {App, Encoder}, AppList).

-spec reg_app_test() -> ok.
reg_app_test() ->
    Apps0 = orddict:new(),
    Apps1 = reg_app(foo, test_pb, Apps0),
    ?assertEqual(true, orddict:is_key(0, Apps1)),
    % Try adding another
    Apps2 = reg_app(bar, test_pb, Apps1),
    ?assertEqual(true, orddict:is_key(1, Apps2)),
    ok.

-spec reg_app_prefix_test() -> ok.
reg_app_prefix_test() ->
    Apps0 = orddict:new(),
    Apps1 = reg_app(10, foo, test_pb, Apps0),
    ?assertEqual(true, orddict:is_key(10, Apps1)),
    % Try adding another to see if it increments properly
    Apps2 = reg_app(bar, test_pb, Apps1),
    ?assertEqual(true, orddict:is_key(11, Apps2)),
    ok.

-spec deeper_propmap(list()) -> map().
deeper_propmap(PropList) ->
    F = fun
        ({K, V}) ->
            proplists:to_map([{K, V}]);
        (List) when is_list(List) ->
            proplists:to_map(List);
        (Atom) when is_atom(Atom) ->
            Atom
    end,
    G = fun(K, V, MapIn) ->
        Map = F(V),
        MapIn#{K => Map}
    end,
    Map = proplists:to_map(PropList),
    maps:fold(G, #{}, Map).

-spec deeper_propmap_test() -> ok.
deeper_propmap_test() ->
    PropList = [
        {foo, true},
        bar,
        {baz, {deep, true}}
    ],
    Map = deeper_propmap(PropList),
    ?assertEqual(true, maps:get(foo, Map)),
    ?assertEqual(true, maps:get(bar, Map)),
    ?assertEqual(#{deep => true}, maps:get(baz, Map)),
    ok.
