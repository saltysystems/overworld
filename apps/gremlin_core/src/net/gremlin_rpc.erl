-module(gremlin_rpc).

% Functions for handling RPCs
-export([
    opcode/1,
    mfa/1,
    client_msg/1,
    server_msg/1,
    encoder/1
]).

-type rpc() :: #{
    opcode := pos_integer(),
    mfa := mfa(),
    client_msg => atom(),
    server_msg => atom(),
    encoder => atom()
}.
-export_type([rpc/0]).

-type callbacks() :: [rpc(), ...].

-export_type([callbacks/0]).

-callback rpc_info() ->
    Callbacks :: [rpc(), ...].

-spec opcode(map()) -> mfa() | undefined.
opcode(Map) ->
    maps:get(opcode, Map, undefined).

-spec mfa(map()) -> mfa() | undefined.
mfa(Map) ->
    maps:get(mfa, Map, undefined).

-spec client_msg(map()) -> atom() | undefined.
client_msg(Map) ->
    maps:get(client_msg, Map, undefined).

-spec server_msg(map()) -> atom() | undefined.
server_msg(Map) ->
    maps:get(server_msg, Map, undefined).

-spec encoder(map()) -> atom() | undefined.
encoder(Map) ->
    maps:get(encoder, Map, undefined).
