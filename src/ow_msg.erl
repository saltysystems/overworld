-module(ow_msg).
-behaviour(ow_rpc).

% Special generic versions if applications implement encode/decode the same way
% that Overworld does it.
-export([encode/3, decode/3]).
% Callbacks specified by the ow_rpc behaviour + debugging decoder
-export([decode/2, raw_decode/1, encode/2]).

-spec decode(binary(), ow_session:session()) -> any().
decode(Msg, Session) ->
    decode(Msg, Session, overworld).

-spec decode(binary(), ow_session:session(), atom()) -> any().
decode(Msg, Session, Application) ->
    ProtoLib = add_pb_suffix(Application),
    % First decode the larger message
    Decoded = erlang:apply(ProtoLib, decode_msg, [Msg, Application]),
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

-spec encode(map(), atom()) -> binary().
encode(SubMsg, Call) ->
    encode(SubMsg, Call, overworld).

-spec encode(map(), atom(), atom()) -> binary().
encode(SubMsg, Call, Application) ->
    Prefix = ow_protocol:prefix(Application),
    B1 = <<Prefix:16>>,
    ProtoLib = add_pb_suffix(Application),
    Msg = #{msg => {Call, SubMsg}},
    B2 = erlang:apply(ProtoLib, encode_msg, [Msg, Application]),
    <<B1/binary, B2/binary>>.

-spec add_pb_suffix(atom()) -> atom().
add_pb_suffix(Application) ->
    list_to_existing_atom(atom_to_list(Application) ++ "_pb").
