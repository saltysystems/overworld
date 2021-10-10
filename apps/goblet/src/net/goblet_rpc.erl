-module(goblet_rpc).

-callback rpc_info() -> Callbacks :: [{pos_integer(), mfa()}, ...].
