-module(ow_router).

-export([defaults/0]).

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
-callback decode(BinMsg, Session) -> Result when
    BinMsg :: binary(),
    Session :: ow_session:session(),
    Result :: any().
-callback decode(BinMsg, Session, Lib, App) -> Result when
    BinMsg :: binary(),
    Session :: ow_session:session(),
    Lib :: atom(),
    App :: atom(),
    Result :: any().
-optional_callbacks([encode/2, decode/4]).
-include("rpc/defaults.hrl").

defaults() ->
    ?DEFAULT_RPC.
