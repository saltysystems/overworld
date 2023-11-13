-module(ow_netfmt).
% @doc Helper module for marshalling various Overworld types into maps for
%      Protobuf consumption

-export([vec2map/1, map2vec/1, to_proto/1]).

%----------------------------------------------------------------------
% Network Encoding/Decoding Functions
%----------------------------------------------------------------------

-type any_vector() ::
    ow_vector:vector2() | ow_vector:vector3() | ow_vector:vector4().

-spec vec2map(any_vector() | [any_vector()]) -> [map()] | map().
vec2map(Vectors) when is_list(Vectors) ->
    [vec2map(Vector) || Vector <- Vectors];
vec2map({X, Y}) ->
    #{x => X, y => Y};
vec2map({X, Y, Z}) ->
    #{x => X, y => Y, z => Z};
vec2map({W, X, Y, Z}) ->
    #{w => W, x => X, y => Y, z => Z}.

-spec map2vec([map()] | map()) -> [any_vector()] | any_vector().
map2vec(Vectors) when is_list(Vectors) ->
    [map2vec(Vector) || Vector <- Vectors];
map2vec(#{w := W, x := X, y := Y, z := Z}) ->
    {W, X, Y, Z};
map2vec(#{x := X, y := Y, z := Z}) ->
    {X, Y, Z};
map2vec(#{x := X, y := Y}) ->
    {X, Y}.

maybe_proto({X, Y}) ->
    vec2map({X, Y});
maybe_proto({X, Y, Z}) ->
    vec2map({X, Y, Z});
maybe_proto({W, X, Y, Z}) ->
    vec2map({W, X, Y, Z});
maybe_proto(V) when is_map(V) ->
    to_proto(V);
maybe_proto(V) ->
    V.

-spec to_proto(map()) -> map().
to_proto(Map) ->
    % Marshall any type not understood by gpb
    F = fun
        (_Key, Val) when is_list(Val) ->
            [maybe_proto(V) || V <- Val];
        (_Key, Val) ->
            maybe_proto(Val)
    end,
    maps:map(F, Map).
