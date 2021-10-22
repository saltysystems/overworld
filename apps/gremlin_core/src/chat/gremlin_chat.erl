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

-spec rpc_info() -> [{pos_integer(), mfa()}, ...].
rpc_info() ->
    [].
