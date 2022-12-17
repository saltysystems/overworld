-module(ow_vector).
% @doc Vector math and other goodies
%      Particularly from:
%       https://github.com/JuantAldea/Separating-Axis-Theorem/blob/master/python/separation_axis_theorem.py

-export([
    add/2,
    subtract/2,
    rotate/2,
    length_squared/1,
    scale/2,
    dot/2,
    cross/2,
    normalize/1,
    orthogonal/1,
    edge_direction/2,
    vertices_to_edges/1,
    project/2,
    overlap/2,
    translate/2,
    intersect/4,
    intersect/5,
    is_collision/2,
    aabb/1,
    test/0,
    test_intersect/0,
    vector_map/1,
    vector_tuple/1,
    rect_to_maps/1,
    rect_to_tuples/1
]).

-define(EPSILON, 1.0e-10).

-type vector() :: {scalar(), scalar()}.
-type vector_map() :: #{x => scalar(), y => scalar()}.
-type scalar() :: number().

-export_type([vector/0, vector_map/0, scalar/0]).

-spec add(vector(), vector()) -> vector().
add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

subtract({X1, Y1}, {X2, Y2}) ->
    {X2 - X1, Y2 - Y1}.

-spec rotate(vector(), scalar()) -> vector().
rotate({X, Y}, RotRad) ->
    CR = math:cos(RotRad),
    SR = math:sin(RotRad),
    {CR * X - SR * Y, SR * X + CR * Y}.

-spec length_squared(vector()) -> float().
length_squared({X, Y}) ->
    math:pow(X, 2) + math:pow(Y, 2).

-spec scale(vector(), scalar()) -> vector().
scale({X, Y}, Scalar) ->
    {X * Scalar, Y * Scalar}.

-spec dot(vector(), vector()) -> scalar().
dot({X1, Y1}, {X2, Y2}) ->
    X1 * X2 + Y1 * Y2.

-spec cross(vector(), vector()) -> scalar().
cross({X1, Y1}, {X2, Y2}) ->
    % the 2D cross product is a mathematical hack :)
    (X1 * Y2) - (Y1 * X2).

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

-spec project([vector(), ...], vector()) -> [scalar(), ...].
project(Vertices, Axis) ->
    % A vector showing how much of the vertices lies along the axis
    Dots = [dot(Vertex, Axis) || Vertex <- Vertices],
    [lists:min(Dots), lists:max(Dots)].

-spec overlap([scalar(), ...], [scalar(), ...]) -> boolean().
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
    Xs = lists:sort(XList),
    Ys = lists:sort(YList),
    [XMin | _] = Xs,
    [YMin | _] = Ys,
    [XMax | _] = lists:last(Xs),
    [YMax | _] = lists:last(Ys),

    % Axis-aligned bounding box.
    [
        {XMin, YMin},
        {XMax, YMin},
        {XMin, YMax},
        {XMax, YMax}
    ].

% If Pos is a tuple, assume tuple mode
% If Pos is a map, assume map mode
translate(Object, Pos) when is_tuple(Pos) ->
    {XNew, YNew} = Pos,
    [{X + XNew, Y + YNew} || {X, Y} <- Object];
translate(Object, Pos) when is_map(Pos) ->
    #{x := XNew, y := YNew} = Pos,
    Fun = fun(Elem, AccIn) ->
        #{x := X, y := Y} = Elem,
        [#{x => X + XNew, y => Y + YNew} | AccIn]
    end,
    lists:foldl(Fun, [], Object).
test() ->
    A = [{0, 0}, {70, 0}, {0, 70}],
    B = [{70, 70}, {150, 70}, {70, 150}],
    C = [{30, 30}, {150, 70}, {70, 150}],

    [
        is_collision(A, B),
        is_collision(A, C),
        is_collision(B, C)
    ].

-spec intersect(vector(), vector(), vector(), vector()) -> boolean().
intersect(A, B, C, D) ->
    intersect(A, B, C, D, lineline).
-spec intersect(
    vector(), vector(), vector(), vector(), rayline | rayray | lineline
) -> boolean().
intersect({Ax, Ay} = A, B, {Cx, Cy} = C, D, LineType) ->
    % Let A and B be two points that constitute a line segment.
    % Let C and D be two more points that constitute another line segment.
    R = subtract(A, B),
    {Rx, Ry} = R,
    S = subtract(C, D),
    {Sx, Sy} = S,
    % calculate the 2d 'cross product' of these segments
    case cross(R, S) of
        0 ->
            % Lines are co-linear
            false;
        RcrossS ->
            U = ((Cx - Ax) * Ry - (Cy - Ay) * Rx) / RcrossS,
            T = ((Cx - Ax) * Sy - (Cy - Ay) * Sx) / RcrossS,
            Intersects =
                case LineType of
                    rayray ->
                        0 =< U andalso 0 =< T;
                    rayline ->
                        0 =< U andalso U =< 1 andalso 0 =< T;
                    lineline ->
                        0 =< U andalso U =< 1 andalso 0 =< T andalso T =< 1
                end,
            case Intersects of
                true ->
                    add(A, scale(R, T));
                false ->
                    false
            end
    end.

test_intersect() ->
    %TODO : Write proper eunit tests
    % Check if parallel lines succeed.
    Ap = {0, 0},
    Bp = {2, 2},
    Cp = {2, 0},
    Dp = {4, 2},
    % Check if coincidental lines succeed
    Ac = {0, 0},
    Bc = {0, 2},
    Cc = {0, 4},
    Dc = {0, 6},
    % Check if crossing lines succeed
    Ax = {0, 0},
    Bx = {2, 2},
    Cx = {2, 0},
    Dx = {0, 2},
    % Check if eventually crossing but not right now lines succeed
    Ae = {0, 0},
    Be = {1, 1},
    Ce = {2, 0},
    De = {2, 2},
    LineLine = [
        intersect(Ap, Bp, Cp, Dp, lineline),
        intersect(Ac, Bc, Cc, Dc, lineline),
        intersect(Ax, Bx, Cx, Dx, lineline),
        intersect(Ae, Be, Ce, De, lineline)
    ],
    RayLine = [
        intersect(Ap, Bp, Cp, Dp, rayline),
        intersect(Ac, Bc, Cc, Dc, rayline),
        intersect(Ax, Bx, Cx, Dx, rayline),
        intersect(Ae, Be, Ce, De, rayline)
    ],
    io:format("Line intersect results: ~p~n", [LineLine]),
    io:format("Ray intersect results: ~p~n", [RayLine]).

%----------------------------------------------------------------------
% Network Encoding/Decoding Functions
%----------------------------------------------------------------------

-spec rect_to_maps([vector()]) -> [map()].
rect_to_maps(Vertices) ->
    [vector_map(Vector) || Vector <- Vertices].

-spec rect_to_tuples([map()]) -> [vector()].
rect_to_tuples(Vertices) ->
    [{X, Y} || #{x := X, y := Y} <- Vertices].

-spec vector_map(vector()) -> map().
vector_map({X, Y}) ->
    #{x => X, y => Y}.

-spec vector_tuple(map()) -> vector().
vector_tuple(#{x := X, y := Y}) ->
    {X, Y}.
