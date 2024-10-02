-module(ow_msg).
-behaviour(ow_router).

% This module does double-duty as a generic encoder/decoder for applications
% (encode/4, decode/4), and also an Overworld-specific application encoder
% (encode/2, decode/2) implementing the behaviour. If application protofiles
% are arranged in the same way as the Overworld protofiles, you can just pass
% through to the arity 4 functions here.

-export([encode/2, decode/2]).
-export([encode/4, decode/4]).
% Debugging decoder
-export([raw_decode/1]).

-spec decode(binary(), pid()) -> any().
%% @doc Decodes a message using the Overworld-specific application decoder.
decode(Msg, SessionPID) ->
    EncoderLib = overworld_pb,
    Application = overworld,
    decode(Msg, SessionPID, EncoderLib, Application).

-spec decode(binary(), pid(), atom(), atom()) -> any().
%% @doc Decodes a message using the specified encoder library and application.
%% First, it decodes the larger message using the encoder library and application.
%% Then, it looks up the appropriate server callback module based on the decoded call.
%% Finally, it sends the sub-message to the sub-module for further processing and
%% returns the result.
decode(Msg, SessionPID, EncoderLib, Application) ->
    % First decode the larger message
    Decoded = erlang:apply(EncoderLib, decode_msg, [Msg, Application]),
    #{msg := {Call, SubMsg}} = Decoded,
    % Lookup the appropriate server callback
    #{module := Module} = ow_protocol:rpc(Call, server),
    % Send the message to the submodule for further processing, and send a
    % reply if any
    erlang:apply(Module, Call, [SubMsg, SessionPID]).

-spec raw_decode(binary()) -> {atom(), map()}.
%% @doc Decodes a raw message and returns the decoded call and sub-message.
%% This function is primarily used for debugging purposes.
raw_decode(Msg) ->
    #{msg := {Call, SubMsg}} = overworld_pb:decode_msg(Msg, overworld),
    {Call, SubMsg}.

-spec encode(map(), atom(), atom(), atom()) -> binary().
%% @doc Encodes a message using the specified encoder library and application.
%% It first retrieves the prefix for the application and prepends it to the encoded message.
encode(SubMsg, Call, EncoderLib, Application) ->
    Prefix = ow_protocol:prefix(Application),
    B1 = <<Prefix:16>>,
    Msg = #{msg => {Call, SubMsg}},
    B2 = erlang:apply(EncoderLib, encode_msg, [Msg, Application]),
    <<B1/binary, B2/binary>>.

-spec encode(map(), atom()) -> binary().
%% @doc Encodes a message using the Overworld-specific application encoder.
%% If called from Overworld, the EncoderLib and Application are known.
encode(SubMsg, Call) ->
    % If being called from Overworld, we know the EncoderLib and Application name
    EncoderLib = overworld_pb,
    Application = overworld,
    encode(SubMsg, Call, EncoderLib, Application).
