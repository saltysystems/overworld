%%=========================================================================
%% Goblet Protocol 2
%%
%% This module handles:
%%  - Registering/deregistering new opcodes for message types
%%  - Routing messages to appropriate modules to deserialization
%%
%%=========================================================================
-module(goblet_protocol2).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

% public API
-export([
    start/0,
    stop/0,
    registered_ops/0,
    register/1,
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

%-include("net/goblet_session.hrl").
-include("db/goblet_database.hrl").
-include("goblet_pb.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-opaque opcode() :: <<_:4>>.

-export_type([opcode/0]).

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
%% @doc Register a new opcode and associated callback function for Module.
%%      Returns {ok, NewState} on success and {{error, Reason}, State}} if
%%      the registration fails.
%% @end
%%-------------------------------------------------------------------------
-spec register(atom()) -> {reply, ok | {error, atom()}, map()}.
register(Module) ->
    gen_server:call(?MODULE, {register, Module}).

%%-------------------------------------------------------------------------
%% @doc Decode messages from clients and route them to an appropriate
%%      function
%% @end
%%-------------------------------------------------------------------------
-spec decode(binary(), goblet_session:session()) ->
    {ok, goblet_session:session()} | {binary(), goblet_session:session()}.
decode(Message, Session) ->
    gen_server:call(?MODULE, {decode, Message, Session}).

%%-------------------------------------------------------------------------
%% @doc Encode a general response
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error) -> binary().
response(ok) ->
    goblet_pb:encode_msg(#'GenResponse'{status = 'OK'});
response(error) ->
    goblet_pb:encode_msg(#'GenResponse'{status = 'ERROR'}).

%%-------------------------------------------------------------------------
%% @doc Get a list of opcodes registered with the server
%% @end
%%-------------------------------------------------------------------------
-spec registered_ops() -> [pos_integer(), ...].
registered_ops() ->
    gen_server:call(?MODULE, registered_ops).

%%-------------------------------------------------------------------------
%% @doc Encode a general response with reasoning
%% @end
%%-------------------------------------------------------------------------
-spec response(ok | error, string()) -> binary().
response(ok, Msg) ->
    goblet_pb:encode_msg(#'GenResponse'{
        status = 'OK',
        msg = Msg
    });
response(error, Msg) ->
    goblet_pb:encode_msg(#'GenResponse'{
        status = 'ERROR',
        msg = Msg
    }).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init([]) ->
    {ok, #{}}.

handle_call({register, Module}, _From, St0) ->
    % Get list of opcodes to register for Module
    RPCs = erlang:apply(Module, rpc_info, []),
    St1 = lists:foldl(fun(Item, Map) -> reg_op(Item, Map) end, St0, RPCs),
    {reply, ok, St1};
handle_call({decode, Message, Session}, _From, St0) ->
    Reply = route(Message, Session, St0),
    {reply, Reply, St0};
handle_call(registered_ops, _From, St0) ->
    Reply = maps:keys(St0),
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
-spec route(<<_:16, _:_*8>>, goblet_session:session(), map()) ->
    goblet_websocket:ws_result().
route(<<OpCode:16, Message/binary>>, Session, Routes) ->
    logger:debug("Got OpCode: ~p~n", [OpCode]),
    case maps:get(OpCode, Routes, unknown) of
        unknown ->
            logger:debug("Got an unknown OpCode: ~p~n", [OpCode]);
        {Module, Fun, 2} ->
            % Apply the callback with the session state and the content of the
            % message
            erlang:apply(Module, Fun, [Message, Session]);
        {Module, Fun, 1} ->
            % Apply the callback with only the content of the message
            erlang:apply(Module, Fun, [Message])
    end.

reg_op({OpCode, Callback}, St0) ->
    % Check if it exists
    case maps:get(OpCode, St0, undefined) of
        undefined ->
            maps:put(OpCode, Callback, St0);
        _ ->
            St0
    end.
