-module(gremlin_vector).
% @doc Vector math and other goodies
%      Particularly from:
%       https://github.com/JuantAldea/Separating-Axis-Theorem/blob/master/python/separation_axis_theorem.py
%      and licensed under the AGPL as a derivative work

-export([
    dot/2,
    normalize/1,
    orthogonal/1,
    edge_direction/2,
    vertices_to_edges/1,
    project/2,
    overlap/2,
    translate/2,
    is_collision/2,
    aabb/1,
    test/0
]).

-type vector() :: {scalar(), scalar()}.
-type scalar() :: number().

-export_type([vector/0, scalar/0]).

-spec dot(vector(), vector()) -> scalar().
dot({X1, Y1}, {X2, Y2}) ->
    X1 * X2 + Y1 * Y2.

-spec normalize(vector()) -> vector().
normalize({X1, Y1}) ->
    N = math:sqrt(
        math:pow(X1, 2) + math:pow(Y1, 2)
    ),
    {X1 / N, Y1 / N}.

-spec orthogonal(vector()) -> vector().
orthogonal({X1, Y1}) ->
    % A vector orthogonal to the input vector
    {-Y1, X1}.

-spec edge_direction(vector(), vector()) -> vector().
edge_direction({X1, Y1}, {X2, Y2}) ->
    % A vector pointing from V1 to V2
    {X2 - X1, Y2 - Y1}.

-spec vertices_to_edges([vector(), ...]) -> [vector(), ...].
vertices_to_edges(Vertices = [First | _Rest]) ->
    % A list of the edges of the vertices as vectors
    vertices_to_edges(Vertices, First, []).

vertices_to_edges([Last], First, Acc) ->
    [edge_direction(Last, First) | Acc];
vertices_to_edges([V1, V2 | Rest], First, Acc) ->
    E = edge_direction(V1, V2),
    vertices_to_edges([V2 | Rest], First, [E | Acc]).

-spec project(vector(), vector()) -> [scalar(), ...].
project(Vertices, Axis) ->
    % A vector showing how much of the vertices lies along the axis
    Dots = [dot(Vertex, Axis) || Vertex <- Vertices],
    [lists:min(Dots), lists:max(Dots)].

-spec overlap([vector(), ...], [vector(), ...]) -> boolean().
overlap(Projection1, Projection2) ->
    Min1 = lists:min(Projection1),
    Min2 = lists:min(Projection2),
    Max1 = lists:max(Projection1),
    Max2 = lists:max(Projection2),
    (Min1 =< Max2) and (Min2 =< Max1).

is_collision(Object1, Object2) ->
    Edges = vertices_to_edges(Object1) ++ vertices_to_edges(Object2),
    Axes = [normalize(orthogonal(Edge)) || Edge <- Edges],
    Overlaps = [detect_overlaps(Object1, Object2, Axis) || Axis <- Axes],
    lists:foldl(fun(Next, SoFar) -> Next and SoFar end, true, Overlaps).

detect_overlaps(Object1, Object2, Axis) ->
    ProjA = project(Object1, Axis),
    ProjB = project(Object2, Axis),
    overlap(ProjA, ProjB).

% Create an axis-aligned bounding box for the entity. This is NOT the minimum
% bounding box, but is cheaper to calculate. It also must be recalculated for
% every rotation of the object.
aabb(Vertices) ->
    XList = [X || {X, _} <- Vertices],
    YList = [Y || {_, Y} <- Vertices],
    % Axis-aligned bounding box.
    [
        {lists:min(XList), lists:min(YList)},
        {lists:max(XList), lists:min(YList)},
        {lists:min(XList), lists:max(YList)},
        {lists:max(XList), lists:max(YList)}
    ].

translate(Object, {Xnew, Ynew}) ->
    [{X + Xnew, Y + Ynew} || {X, Y} <- Object].

test() ->
    A = [{0, 0}, {70, 0}, {0, 70}],
    B = [{70, 70}, {150, 70}, {70, 150}],
    C = [{30, 30}, {150, 70}, {70, 150}],

    [
        is_collision(A, B),
        is_collision(A, C),
        is_collision(B, C)
    ].
