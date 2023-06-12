-module(ow_msg).
-behaviour(ow_rpc).

% This module does double-duty as a generic encoder/decoder for applications
% (encode/4, decode/4), and also an Overworld-specific application encoder
% (encode/2, decode/2) implementing the behaviour. If application protofiles
% are arranged in the same way as the Overworld protofiles, you can just pass
% through to the arity 4 functions here.

-export([encode/2, decode/2]).
-export([encode/4, decode/4]).
% Debugging decoder
-export([raw_decode/1]).

-spec decode(binary(), ow_session:session()) -> any().
decode(Msg, Session) ->
    EncoderLib = overworld_pb,
    Application = overworld,
    decode(Msg, Session, EncoderLib, Application).

-spec decode(binary(), ow_session:session(), atom(), atom()) -> any().
decode(Msg, Session, EncoderLib, Application) ->
    % First decode the larger message
    Decoded = erlang:apply(EncoderLib, decode_msg, [Msg, Application]),
    #{msg := {Call, SubMsg}} = Decoded,
    % Lookup the appropriate server callback
    #{module := Module} = ow_protocol:rpc(Call, server),
    % Send the message to the submodule for further processing, and send a
    % reply if any
    erlang:apply(Module, Call, [SubMsg, Session]).

-spec raw_decode(binary()) -> {atom(), map()}.
raw_decode(Msg) ->
    #{msg := {Call, SubMsg}} = overworld_pb:decode_msg(Msg, overworld),
    {Call, SubMsg}.

-spec encode(map(), atom(), atom(), atom()) -> binary().
encode(SubMsg, Call, EncoderLib, Application) ->
    Prefix = ow_protocol:prefix(Application),
    B1 = <<Prefix:16>>,
    Msg = #{msg => {Call, SubMsg}},
    B2 = erlang:apply(EncoderLib, encode_msg, [Msg, Application]),
    <<B1/binary, B2/binary>>.

-spec encode(map(), atom()) -> binary().
encode(SubMsg, Call) ->
    % If being called from Overworld, we know the EncoderLib and Application name
    EncoderLib = overworld_pb,
    Application = overworld,
    encode(SubMsg, Call, EncoderLib, Application).