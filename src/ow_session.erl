%%=========================================================================
%% Overworld Session
%%
%% This module handles communication with game sessions.
%%
%%=========================================================================
-module(ow_session).

-behaviour(ow_rpc).

% Required ow callbacks
-export([rpc_info/0]).

-export([
    encode_log/1,
    broadcast/2,
    multicast/2,
    new/0,
    set_id/2,
    get_id/1,
    set_pid/2,
    get_pid/1,
    set_serializer/2,
    get_serializer/1,
    set_authenticated/2,
    get_authenticated/1,
    set_latency/2,
    get_latency/1,
    % alias for get_authenticated
    is_authenticated/1,
    set_game_info/2,
    get_game_info/1,
    set_termination_callback/2,
    get_termination_callback/1,
    session_id_req/2,
    session_ping/2,
    session_pong/1,
    version/1
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-type msg() :: nonempty_binary() | [binary(), ...].
-type serializer() :: 'undefined' | 'protobuf'.

-record(session, {
    id :: integer(),
    pid :: pid() | undefined,
    serializer :: serializer(),
    authenticated = false :: boolean(),
    % ms
    latency = 0 :: non_neg_integer(),
    % it can be anything but we default to map
    game_info = #{} :: term(),
    termination_callback :: mfa() | undefined
}).

-opaque session() :: #session{}.
-opaque state_update() :: session() | {iodata(), session()}.

-export_type([session/0]).
-export_type([state_update/0]).
-export_type([serializer/0]).

-define(PROTOCOLVERSION, 1).

%%===========================================================================
%% Reserved OpCodes
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Required callback for Overworld. Register the ?ACCOUNT_NEW and
%%      ?ACCOUNT_LOGIN opcodes and associated functions.
%% @end
%%----------------------------------------------------------------------------

-define(VERSION, 16#0010).
-define(SESSION_ID_REQ, 16#0015).
-define(SESSION_ID, 16#0016).
-define(SESSION_PING, 16#0020).
-define(SESSION_PONG, 16#0021).
-define(SESSION_LOG, 16#0050).

-spec rpc_info() -> ow_rpc:callbacks().
rpc_info() ->
    [
        #{
            opcode => ?VERSION,
            c2s_handler => {?MODULE, version, 1},
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?SESSION_ID_REQ,
            c2s_handler => {?MODULE, session_id_req, 2},
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?SESSION_ID,
            s2c_call => session_id,
            encoder => overworld_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?SESSION_PING,
            c2s_handler => {?MODULE, session_ping, 2},
            encoder => overworld_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?SESSION_PONG,
            s2c_call => session_pong,
            encoder => overworld_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?SESSION_LOG,
            s2c_call => session_log,
            encoder => overworld_pb,
            qos => reliable,
            channel => 0
        }
    ].

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Update the latency
%% @end
%%----------------------------------------------------------------------------
session_ping(Msg, Session) ->
    D = overworld_pb:decode_msg(Msg, session_ping),
    ID = maps:get(id, D),
    Last = ow_beacon:get_by_id(ID),
    Now = erlang:monotonic_time(),
    Latency = erlang:convert_time_unit(
        round((Now - Last) / 2), native, millisecond
    ),
    Session1 = set_latency(Latency, Session),
    Resp = overworld_pb:encode_msg(#{latency => Latency}, session_pong),
    {[<<?SESSION_PONG:16>>, Resp], Session1}.

%%----------------------------------------------------------------------------
%% @doc Receives PONGs from the Client. No op - this is basically a stub to
%%      ensure proper API generation.
%% @end
%%----------------------------------------------------------------------------
session_pong(_) ->
    ok.

%%----------------------------------------------------------------------------
%% @doc Receives VERSION requests
%% @end
%%----------------------------------------------------------------------------
version(_S) ->
    logger:debug("Got a VERSION request"),
    ok.

%%----------------------------------------------------------------------------
%% @doc Return the session ID back to the caller
%% @end
%%----------------------------------------------------------------------------
session_id_req(_Msg, Session) ->
    ID = ow_session:get_id(Session),
    Resp = overworld_pb:encode_msg(#{id => ID}, session_id),
    {[<<?SESSION_ID:16>>, Resp], Session}.

%%----------------------------------------------------------------------------
%% @doc Encodes a log message to be sent back to the client
%% @end
%%----------------------------------------------------------------------------
-spec encode_log(string()) -> msg().
encode_log(Message) ->
    OpCode = <<?SESSION_LOG:16>>,
    Sanitized = sanitize_message(Message),
    % Send a 1-byte color message in hex - default black
    Color = <<000000:8>>,
    Msg = overworld_pb:encode_msg(
        #{color => Color, msg => Sanitized}, session_log
    ),
    [OpCode, Msg].

-spec encode_log_test() -> ok.
encode_log_test() ->
    OriginalMessage = "Hello World",
    [OpCode, Message] = encode_log(OriginalMessage),
    % Check that the expected OpCode comes back
    ?assertEqual(OpCode, <<?SESSION_LOG:16>>),
    % Check that the message decodes correctly
    Decoded = overworld_pb:decode_msg(Message, session_log),
    ToList = maps:get(msg, Decoded),
    ?assertEqual(OriginalMessage, ToList),
    ok.

%%----------------------------------------------------------------------------
%% @doc Broadcast a message to all clients. Must already be serialized.
%% @end
%%----------------------------------------------------------------------------
-spec broadcast(msg(), {atom(), non_neg_integer() | undefined}) -> ok.
broadcast(EncodedMsg, Options) ->
    case gproc:lookup_pids({p, l, client_session}) of
        [] ->
            ok;
        _ ->
            gproc:send({p, l, client_session}, {
                self(), broadcast, EncodedMsg, Options
            })
    end,
    ok.

%%----------------------------------------------------------------------------
%% @doc Send a message to one or more clients. Must already be serialized.
%% @end
%%----------------------------------------------------------------------------
-spec multicast(msg(), [integer(), ...]) -> ok.
multicast(EncodedMsg, SessionIDs) ->
    % This is actually just sending all client sessions a message, and
    % we filter before sending it to the actual client. Probably a bottleneck?
    gproc:send(
        {p, l, client_session},
        {self(), multicast, EncodedMsg, SessionIDs}
    ),
    ok.

%%----------------------------------------------------------------------------
%% @doc New session
%% @end
%%----------------------------------------------------------------------------
-spec new() -> session().
new() ->
    #session{id = erlang:unique_integer()}.

%%----------------------------------------------------------------------------
%% @doc Set the session ID
%% @end
%%----------------------------------------------------------------------------
-spec set_id(integer(), session()) -> session().
set_id(ID, Session) ->
    Session#session{id = ID}.

%%----------------------------------------------------------------------------
%% @doc Return the session ID
%% @end
%%----------------------------------------------------------------------------
-spec get_id(session()) -> integer().
get_id(Session) ->
    Session#session.id.

%%----------------------------------------------------------------------------
%% @doc Set the socket PID
%% @end
%%----------------------------------------------------------------------------
-spec set_pid(pid(), session()) -> session().
set_pid(PID, Session) ->
    Session#session{pid = PID}.

%%----------------------------------------------------------------------------
%% @doc Return the socket PID
%% @end
%%----------------------------------------------------------------------------
-spec get_pid(session()) -> pid().
get_pid(Session) ->
    Session#session.pid.

%%----------------------------------------------------------------------------
%% @doc Set the format for serializing data. If communication happens all
%%      within Erlang node(s), then there is no need to set a serializer.
%% @end
%%----------------------------------------------------------------------------
-spec set_serializer(serializer(), session()) -> session().
set_serializer(Serializer, Session) ->
    Session#session{serializer = Serializer}.

%%----------------------------------------------------------------------------
%% @doc Return the session serializer.
%% @end
%%----------------------------------------------------------------------------
-spec get_serializer(session()) -> serializer().
get_serializer(Session) ->
    Session#session.serializer.

%%----------------------------------------------------------------------------
%% @doc Set the session latency
%% @end
%%----------------------------------------------------------------------------
-spec set_latency(pos_integer(), session()) -> session().
set_latency(Latency, Session) ->
    Session#session{latency = Latency}.

%%----------------------------------------------------------------------------
%% @doc Return the session latency
%% @end
%%----------------------------------------------------------------------------
-spec get_latency(session()) -> non_neg_integer().
get_latency(Session) ->
    Session#session.latency.

%%----------------------------------------------------------------------------
%% @doc Set the session authentication status
%% @end
%%----------------------------------------------------------------------------
-spec set_authenticated(boolean(), session()) -> session().
set_authenticated(Bool, Session) ->
    Session#session{authenticated = Bool}.

%%----------------------------------------------------------------------------
%% @doc Return the session authentication status
%% @end
%%----------------------------------------------------------------------------
-spec get_authenticated(session()) -> boolean().
get_authenticated(Session) ->
    Session#session.authenticated.

%%----------------------------------------------------------------------------
%% @doc Return the session authentication status. Alias for get_authenticated
%% @end
%%----------------------------------------------------------------------------
-spec is_authenticated(session()) -> boolean().
is_authenticated(Session) ->
    get_authenticated(Session).

%%----------------------------------------------------------------------------
%% @doc Set the game info map
%% @end
%%----------------------------------------------------------------------------
-spec set_game_info(map(), session()) -> session().
set_game_info(Map, Session) ->
    Session#session{game_info = Map}.

%%----------------------------------------------------------------------------
%% @doc Return the game info map
%% @end
%%----------------------------------------------------------------------------
-spec get_game_info(session()) -> map().
get_game_info(Session) ->
    Session#session.game_info.

%%----------------------------------------------------------------------------
%% @doc Set the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec set_termination_callback(mfa(), session()) -> session().
set_termination_callback(Callback, Session) ->
    Session#session{termination_callback = Callback}.

%%----------------------------------------------------------------------------
%% @doc Get the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec get_termination_callback(session()) -> undefined | mfa().
get_termination_callback(Session) ->
    Session#session.termination_callback.

%%=========================================================================
%% Internal functions
%%=========================================================================

sanitize_message(Message) ->
    %TODO: Check for message lengths, etc. Ensure that a client isn't DOSing
    %      other client(s)
    Message.
