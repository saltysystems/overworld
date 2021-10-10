-module(goblet_chat).

-behaviour(goblet_rpc).

-include("goblet_pb.hrl").

% required callbacks for goblet
-export([rpc_info/0]).

% Chat messages, incl. PMs and ignore
-define(CHAT_WHO, 16#0200).
-define(CHAT_JOIN, 16#0210).
-define(CHAT_MSG, 16#0220).
-define(CHAT_WHISPER, 16#0230).
-define(CHAT_SYSTEM_MSG, 16#0240).

-spec rpc_info() -> [{pos_integer(), mfa()}, ...].
rpc_info() ->
    [
        {?CHAT_WHO, {?MODULE, who, 1}},
        {?CHAT_JOIN, {?MODULE, join, 2}},
        {?CHAT_MSG, {?MODULE, msg, 2}},
        {?CHAT_SYSTEM_MSG, {?MODULE, system_msg, 2}},
        {?CHAT_WHISPER, {?MODULE, whisper, 2}}
    ].

% TODO - draw the rest of the fscking owl
