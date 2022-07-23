-module(ow_rpc).

% Functions for handling RPCs
-export([
    opcode/1,
    c2s_handler/1,
    c2s_proto/1,
    s2c_call/1,
    encoder/1,
    find_call/2,
    find_handler/2
]).

-type rpc() :: #{
    opcode := 16#0000..16#FFFF,
    c2s_handler => mfa() | undefined,
    c2s_proto => atom() | undefined,
    s2c_call => atom() | undefined,
    encoder := atom() | undefined
}.
-export_type([rpc/0]).

-type callbacks() :: [rpc(), ...].

-export_type([callbacks/0]).

-callback rpc_info() ->
    Callbacks :: [rpc(), ...].

-spec opcode(map()) -> 16#0000..16#FFFF.
opcode(Map) ->
    maps:get(opcode, Map, undefined).

-spec c2s_handler(map()) -> mfa() | undefined.
c2s_handler(Map) ->
    maps:get(c2s_handler, Map, undefined).

-spec c2s_proto(map()) -> atom() | undefined.
c2s_proto(Map) ->
    maps:get(c2s_proto, Map, undefined).

-spec s2c_call(map()) -> atom() | undefined.
s2c_call(Map) ->
    maps:get(s2c_call, Map, undefined).

-spec encoder(map()) -> atom() | undefined.
encoder(Map) ->
    maps:get(encoder, Map, undefined).

-spec find_call(atom(), [rpc(), ...]) -> rpc() | #{}.
find_call(Msg, [H | L]) ->
    case maps:get(s2c_call, H, undefined) of
        Msg -> H;
        _ -> find_call(Msg, L)
    end;
find_call(_, []) ->
    #{}.

-spec find_handler(atom(), [rpc(), ...]) -> rpc() | #{}.
find_handler(Msg, [H | L]) ->
    % Check first to see if the Fun has been overriden by c2s_proto
    case maps:get(c2s_proto, H, undefined) of
        Msg ->
            H;
        _ ->
            case maps:get(c2s_handler, H, undefined) of
                {_, Msg, _} -> H;
                _ -> find_handler(Msg, L)
            end
    end;
find_handler(_, []) ->
    #{}.
