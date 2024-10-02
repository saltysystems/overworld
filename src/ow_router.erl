-module(ow_router).

-export([defaults/0]).

-define(DEFAULT_RPC, #{qos => reliable, channel => 0}).

-callback encode(Msg, Type) -> Result when
    Msg :: map(),
    Type :: atom(),
    Result :: binary().
-callback encode(Msg, Type, Lib, App) -> Result when
    Msg :: map(),
    Type :: atom(),
    Lib :: atom(),
    App :: atom(),
    Result :: binary().
-callback decode(BinMsg, SessionPID) -> Result when
    BinMsg :: binary(),
    SessionPID :: pid(),
    Result :: any().
-callback decode(BinMsg, SessionPID, Lib, App) -> Result when
    BinMsg :: binary(),
    SessionPID :: pid(),
    Lib :: atom(),
    App :: atom(),
    Result :: any().
-optional_callbacks([encode/2, decode/4]).
-hank([{unused_callbacks, [all]}]).

defaults() ->
    ?DEFAULT_RPC.
