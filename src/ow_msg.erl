-module(ow_msg).
-behaviour(ow_rpc).

-export([encoder/0, decode/2, raw_decode/1, encode/2]).

-spec encoder() -> atom().
encoder() -> overworld_pb.

-spec decode(binary(), ow_session:session()) -> any().
decode(Msg, Session) ->
    % First decode the larger message
    #{msg := {Call, SubMsg}} = overworld_pb:decode_msg(Msg, overworld),
    % Lookup the appropriate server callback
    #{module := Module} = ow_protocol:rpc(Call, server),
    % Send the message to the submodule for further processing, and send a
    % reply if any
    erlang:apply(Module, Call, [SubMsg, Session]).

-spec raw_decode(binary()) -> {atom(), map()}.
raw_decode(Msg) ->
    #{msg := {Call, SubMsg}} = overworld_pb:decode_msg(Msg, overworld),
    {Call, SubMsg}.

-spec encode(map(), atom()) -> binary().
encode(Msg, Type) ->
    Prefix = ow_protocol:prefix(overworld),
    B1 = <<Prefix:16>>,
    B2 = overworld_pb:encode_msg(#{msg => {Type, Msg}}, overworld),
    <<B1/binary, B2/binary>>.
