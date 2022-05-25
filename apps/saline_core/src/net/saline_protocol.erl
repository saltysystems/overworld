%%=========================================================================
%% Gremlin Protocol
%%
%% This module handles:
%%  - Registering/deregistering new opcodes for message types
%%  - Routing messages to appropriate modules to deserialization
%%
%%=========================================================================
-module(saline_protocol).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

% public API
-export([
    start/1,
    stop/0,
    registered_apps/0,
    registered_ops/0,
    register/1,
    register_app/1,
    op_info/1,
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

-include("db/saline_database.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-opaque opcode() :: <<_:16>>.

-export_type([opcode/0]).

%%=========================================================================
%% API
%%=========================================================================

%%-------------------------------------------------------------------------
%% @doc Start the gen_server
%% @end
%%-------------------------------------------------------------------------
-spec start([atom(), ...]) -> {ok, pid()} | ignore | {error, term()}.
start(Modules) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Modules], []).

%%-------------------------------------------------------------------------
%% @doc Stop the gen_server
%% @end
%%-------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%-------------------------------------------------------------------------
%% @doc Register a new Erlang application with Gremlin. This is required
%%      for Gremlin to generate a downloadable zips of protobuf, etc
%%      files.  Returns {ok, NewState} on success and {{error, Reason},
%%      State}} if the registration fails.
%% @end
%%-------------------------------------------------------------------------
-spec register_app(atom()) -> {reply, ok | {error, atom()}, map()}.
register_app(Application) ->
    logger:info("Registering application: ~p~n", [Application]),
    gen_server:call(?MODULE, {register_app, Application}).

%%-------------------------------------------------------------------------
%% @doc Register a new opcode and associated callback function for Module.
%%      Returns {ok, NewState} on success and {{error, Reason}, State}} if
%%      the registration fails.
%% @end
%%-------------------------------------------------------------------------
-spec register(atom()) -> {reply, ok | {error, atom()}, map()}.
register(Module) ->
    logger:info("Registering callbacks for : ~p~n", [Module]),
    gen_server:call(?MODULE, {register_op, Module}).

%%-------------------------------------------------------------------------
%% @doc Decode messages from clients and route them to an appropriate
%%      function
%% @end
%%-------------------------------------------------------------------------
-spec decode(binary(), saline_session:session()) ->
    ok
    | {ok, saline_session:session()}
    | {binary(), saline_session:session()}.
decode(Message, Session) ->
    gen_server:call(?MODULE, {decode, Message, Session}).

%%-------------------------------------------------------------------------
%% @doc Encode a general response
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error) -> binary().
response(ok) ->
    saline_pb:encode_msg(#{status => 'OK'}, gen_response);
response(error) ->
    saline_pb:encode_msg(#{status => 'ERROR'}, gen_response).

%%-------------------------------------------------------------------------
%% @doc Encode a general response with reasoning
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error, string()) -> binary().
response(ok, Msg) ->
    saline_pb:encode_msg(
        #{
            status => 'OK',
            msg => Msg
        },
        gen_response
    );
response(error, Msg) ->
    saline_pb:encode_msg(
        #{
            status => 'ERROR',
            msg => Msg
        },
        gen_response
    ).

%%-------------------------------------------------------------------------
%% @doc Get a list of opcodes registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec registered_ops() -> [pos_integer(), ...].
registered_ops() ->
    gen_server:call(?MODULE, registered_ops).

%%-------------------------------------------------------------------------
%% @doc Get a list of Gremlin games registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec registered_apps() -> [atom(), ...].
registered_apps() ->
    gen_server:call(?MODULE, registered_apps).

%%-------------------------------------------------------------------------
%% @doc Return the MFA for a particular opcode
%% @end
%%-------------------------------------------------------------------------
-spec op_info(pos_integer()) -> saline_rpc:rpc().
op_info(OpCode) ->
    gen_server:call(?MODULE, {op_info, OpCode}).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init([Modules]) ->
    % Register all functions defined in ?INITIAL_MODULES
    InitialOps = lists:foldl(
        fun(Item, Map) -> reg(Item, Map) end,
        #{},
        Modules
    ),
    InitialApps = [saline],
    InitialState = #{ops => InitialOps, apps => InitialApps},
    {ok, InitialState}.

handle_call({register_op, Module}, _From, St0) ->
    % Get list of opcodes to register for Module
    Ops0 = maps:get(ops, St0),
    Ops1 = reg(Module, Ops0),
    {reply, ok, St0#{ops => Ops1}};
handle_call({register_app, Application}, _From, St0) ->
    % Get list of opcodes to register for Module
    Apps0 = maps:get(apps, St0),
    Apps1 = reg_app(Application, Apps0),
    {reply, ok, St0#{apps => Apps1}};
handle_call({decode, Message, Session}, _From, St0) ->
    Reply = route(Message, Session, St0),
    {reply, Reply, St0};
handle_call(registered_ops, _From, St0) ->
    Ops = maps:get(ops, St0),
    Reply = maps:keys(Ops),
    {reply, Reply, St0};
handle_call(registered_apps, _From, St0) ->
    Apps = maps:get(apps, St0),
    {reply, Apps, St0};
handle_call({op_info, OpCode}, _From, St0) ->
    Ops = maps:get(ops, St0),
    Reply = maps:get(OpCode, Ops, undefined),
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
-spec route(<<_:16, _:_*8>>, saline_session:session(), map()) ->
    saline_session:net_msg().
route(<<OpCode:16, Message/binary>>, Session, St0) ->
    Routes = maps:get(ops, St0),
    case maps:get(OpCode, Routes, unknown) of
        unknown ->
            logger:notice("Got an unknown OpCode: ~p", [OpCode]);
        Map ->
            case saline_rpc:c2s_handler(Map) of
                {Module, Fun, 2} ->
                    % Apply the callback with the rest of the message plus
                    % current session information (authentication status, etc).
                    erlang:apply(Module, Fun, [Message, Session]);
                {Module, Fun, 1} ->
                    % The callback is called "sessionless" - e.g. heartbeats,
                    % version request, etc.
                    erlang:apply(Module, Fun, [Message]),
                    % return the session
                    {ok, Session};
                undefined ->
                    % There is no 'C' in this RPC.
                    logger:notice("No RPC registered for ~p", [OpCode]),
                    {ok, Session}
            end
    end.

reg(Module, Ops) ->
    RPCs = erlang:apply(Module, rpc_info, []),
    lists:foldl(fun(Item, Map) -> reg_op(Item, Map) end, Ops, RPCs).

reg_app(App, AppList) ->
    case lists:member(App, AppList) of
        true ->
            AppList;
        false ->
            [App | AppList]
    end.

reg_op(Map, St0) ->
    % Get the OpCode from the inner map
    OpCode = maps:get(opcode, Map),
    case maps:get(OpCode, St0, undefined) of
        undefined ->
            maps:put(OpCode, Map, St0);
        _ ->
            St0
    end.
