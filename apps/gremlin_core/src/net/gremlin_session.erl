%%=========================================================================
%% Gremlin Session
%%
%% This module handles communication with game sessions.
%%
%%=========================================================================
-module(gremlin_session).

-behaviour(gremlin_rpc).

% Required gremlin callbacks
-export([rpc_info/0]).

-export([
    encode_log/1,
    broadcast/1,
    multicast/2,
    new/0,
    set_id/2,
    get_id/1,
    set_pid/2,
    get_pid/1,
    set_type/2,
    get_type/1,
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
    ping/2,
    pong/1,
    version/1
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-type msg() :: nonempty_binary() | [binary(), ...].
-type player_type() :: 'pc' | 'npc'.

-record(session, {
    id :: integer() | undefined,
    pid :: pid() | undefined,
    type :: player_type(),
    authenticated = false :: boolean(),
    % ms
    latency = 0 :: non_neg_integer(),
    game_info :: term() | undefined,
    termination_callback :: mfa() | undefined
}).

-opaque session() :: #session{}.
-opaque state_update() :: session() | {iodata(), session()}.

-export_type([session/0]).
-export_type([state_update/0]).

-define(PROTOCOLVERSION, 1).

%%===========================================================================
%% Reserved OpCodes
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Required callback for Gremlin. Register the ?ACCOUNT_NEW and
%%      ?ACCOUNT_LOGIN opcodes and associated functions.
%% @end
%%----------------------------------------------------------------------------

-define(RPC(OpCode, Callback, Arity, ProtoMessage),
    {OpCode, {{?MODULE, Callback, Arity}, {gremlin_pb, ProtoMessage}}}
).
-define(VERSION, 16#0010).
-define(SESSION_ID_REQ, 16#0015).
-define(SESSION_ID, 16#0016).
-define(SESSION_PING, 16#0020).
-define(SESSION_PONG, 16#0021).
-define(SESSION_LOG, 16#0050).

-spec rpc_info() -> gremlin_rpc:callbacks().
rpc_info() ->
    [
        #{
            opcode => ?VERSION,
            c2s_handler => {?MODULE, version, 1}
        },
        #{
            opcode => ?SESSION_ID_REQ,
            c2s_handler => {?MODULE, session_id_req, 2},
            c2s_call => session_id_req
        },
        #{
            opcode => ?SESSION_ID,
            s2c_call => session_id,
            encoder => gremlin_pb
        },
        #{
            opcode => ?SESSION_PING,
            c2s_handler => {?MODULE, ping, 2},
            c2s_call => session_ping,
            encoder => gremlin_pb
        },
        #{
            opcode => ?SESSION_PONG,
            s2c_call => session_pong,
            encoder => gremlin_pb
        },
        #{
            opcode => ?SESSION_LOG,
            s2c_call => session_log,
            encoder => gremlin_pb
        }
    ].

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Update the latency
%% @end
%%----------------------------------------------------------------------------
ping(Msg, Session) ->
    D = gremlin_pb:decode_msg(Msg, session_ping),
    ID = maps:get(id, D),
    Last = gremlin_beacon:get_by_id(ID),
    Now = erlang:monotonic_time(),
    Latency = erlang:convert_time_unit(
        round((Now - Last) / 2), native, millisecond
    ),
    Session1 = set_latency(Latency, Session),
    Resp = gremlin_pb:encode_msg(#{latency => Latency}, session_pong),
    {[<<?SESSION_PONG:16>>, Resp], Session1}.

%%----------------------------------------------------------------------------
%% @doc Receives PONGs from the Client. No op - this is basically a stub to
%%      ensure proper API generation.
%% @end
%%----------------------------------------------------------------------------
pong(_) ->
    ok.

%%----------------------------------------------------------------------------
%% @doc Receives VERSION requests
%% @end
%%----------------------------------------------------------------------------
version(_) ->
    ok.

%%----------------------------------------------------------------------------
%% @doc Return the session ID back to the caller
%% @end
%%----------------------------------------------------------------------------
session_id_req(_Msg, Session) ->
    ID = gremlin_session:get_id(Session),
    Resp = gremlin_pb:encode_msg(#{id => ID}, session_id),
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
    Msg = gremlin_pb:encode_msg(
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
    Decoded = gremlin_pb:decode_msg(Message, session_log),
    ToList = maps:get(msg, Decoded),
    ?assertEqual(OriginalMessage, ToList),
    ok.

%%----------------------------------------------------------------------------
%% @doc Broadcast a message to all clients. Must already be serialized.
%% @end
%%----------------------------------------------------------------------------
-spec broadcast(msg()) -> ok.
broadcast(EncodedMsg) ->
    case gproc:lookup_pids({p, l, client_session}) of
        [] ->
            ok;
        _ ->
            gproc:send({p, l, client_session}, {
                self(), broadcast, EncodedMsg
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
    #session{}.

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
%% @doc Set the type of session, e.g. player or non-player
%% @end
%%----------------------------------------------------------------------------
-spec set_type(player_type(), session()) -> session().
set_type(Type, Session) ->
    Session#session{type = Type}.

%%----------------------------------------------------------------------------
%% @doc Return the session type
%% @end
%%----------------------------------------------------------------------------
-spec get_type(session()) -> player_type().
get_type(Session) ->
    Session#session.type.

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
-spec set_termination_callback(mfa(), session()) -> map().
set_termination_callback(Callback, Session) ->
    Session#session{termination_callback = Callback}.

%%----------------------------------------------------------------------------
%% @doc Get the termination callback
%% @end
%%----------------------------------------------------------------------------
-spec get_termination_callback(session()) -> map().
get_termination_callback(Session) ->
    Session#session.termination_callback.

%%=========================================================================
%% Internal functions
%%=========================================================================

sanitize_message(Message) ->
    %TODO: Check for message lengths, etc. Ensure that a client isn't DOSing
    %      other client(s)
    Message.
