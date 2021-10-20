-module(gremlin_chat).

-behaviour(gremlin_rpc).

% required callbacks for gremlin
-export([rpc_info/0]).

% Chat messages, incl. PMs and ignore
-define(CHAT_WHO, 16#0200).
-define(CHAT_LIST, 16#0205).
-define(CHAT_JOIN, 16#0210).
-define(CHAT_PART, 16#0220).
-define(CHAT_MSG, 16#0230).
-define(CHAT_WHISPER, 16#0240).
-define(CHAT_SYSTEM_MSG, 16#0250).

-define(RPC(OpCode, Callback, Arity, ProtoMessage),
            {OpCode, {{?MODULE, Callback, Arity}, {gremlin_pb, ProtoMessage}}}
             ).


-spec rpc_info() -> [{pos_integer(), mfa()}, ...].
rpc_info() ->
    [
        ?RPC(?CHAT_WHO, who, 1, none),
        ?RPC(?CHAT_LIST, list, 1, none),
        ?RPC(?CHAT_JOIN, join, 1, none),
        ?RPC(?CHAT_PART, part, 1, none),
        ?RPC(?CHAT_MSG, msg, 1, none),
        ?RPC(?CHAT_SYSTEM_MSG, system_msg, 1, none),
        ?RPC(?CHAT_WHISPER, whisper, 1, none)
    ].
