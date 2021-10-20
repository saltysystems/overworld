-module(gremlin_rpc).

-callback rpc_info() -> Callbacks :: [{pos_integer(), mfa(), atom()}, ...].
