-module(ow_rpc).

-export([defaults/0]).

-callback encode(Msg, Type) -> Result when
    Msg :: map(),
    Type :: atom(),
    Result :: binary().
-callback decode(BinMsg, Session) -> Result when
    BinMsg :: binary(),
    Session :: ow_session:session(),
    Result :: any().

-include("rpc/defaults.hrl").

defaults() ->
    ?DEFAULT_RPC.
