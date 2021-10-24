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
    set_email/2,
    get_email/1,
    set_authenticated/2,
    get_authenticated/1,
    % alias for get_authenticated
    is_authenticated/1,
    set_game_info/2,
    get_game_info/1
]).

-include("db/gremlin_database.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-type msg() :: [binary(), ...].

-record(session, {
    id :: integer() | undefined,
    email :: string() | undefined,
    authenticated = false :: boolean(),
    game_info = #{} :: map() | undefined
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
-define(HEARTBEAT, 16#0020).
-define(SESSION_LOG, 16#0050).

-spec rpc_info() -> [{{pos_integer(), mfa()}, atom()}, ...].
rpc_info() ->
    [
        #{
            opcode => ?VERSION, mfa => {?MODULE, version, 1}
        },
        #{
            opcode => ?HEARTBEAT, mfa => {?MODULE, heartbeat, 1}
        },
        #{
            opcode => ?SESSION_LOG,
            mfa => {?MODULE, session_log, 2},
            server_msg => session_log,
            encoder => gremlin_pb
        }
    ].

%%===========================================================================
%% API
%%===========================================================================

%%----------------------------------------------------------------------------
%% @doc Encodes a log message to be sent back to the client
%% @end
%%----------------------------------------------------------------------------
-spec encode_log(string()) -> msg().
encode_log(Message) ->
    OpCode = <<?SESSION_LOG:16>>,
    Sanitized = sanitize_message(Message),
    Msg = gremlin_pb:encode_msg(#{msg => Sanitized}, session_log),
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
    gproc:send({p, l, client_session}, {self(), broadcast, EncodedMsg}),
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
%% @doc Set the session email
%% @end
%%----------------------------------------------------------------------------
-spec set_email(string(), session()) -> session().
set_email(Email, Session) ->
    Session#session{email = Email}.

%%----------------------------------------------------------------------------
%% @doc Return the session email
%% @end
%%----------------------------------------------------------------------------
-spec get_email(session()) -> string().
get_email(Session) ->
    Session#session.email.

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

%%=========================================================================
%% Internal functions
%%=========================================================================

sanitize_message(Message) ->
    %TODO: Check for message lengths, etc. Ensure that a client isn't DOSing
    %      other client(s)
    Message.
