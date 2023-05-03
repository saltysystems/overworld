-module(ow_vector).
% @doc Vector math and other goodies
%      Particularly from:
%       https://github.com/JuantAldea/Separating-Axis-Theorem/blob/master/python/separation_axis_theorem.py

-export([
    add/2,
    subtract/2,
    rotate/2,
    rotate/3,
    rotate_polygon/2,
    rotate_polygon/3,
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
    line_of_sight/4,
    ray_between/3,
    ray_intersect/4,
    intersect/4,
    intersect/5,
    edges/1,
    outer_edges/1,
    is_collision/2,
    aabb/1,
    test/0,
    test_intersect/0,
    distance/2,
    component_sort/2,
    ysort/1,
    convex_hull/1
]).

-define(EPSILON, 1.0e-10).

-type vector() :: {scalar(), scalar()}.
-type vector3() :: {scalar(), scalar(), scalar()}.
-type vector4() :: {scalar(), scalar(), scalar(), scalar()}.
-type vector_map() :: #{x => scalar(), y => scalar()}.
-type scalar() :: number().

-export_type([vector/0, vector3/0, vector4/0, vector_map/0, scalar/0]).

-spec add(vector(), vector()) -> vector().
add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

subtract({X1, Y1}, {X2, Y2}) ->
    {X2 - X1, Y2 - Y1}.

-spec rotate(vector(), scalar()) -> vector().
rotate({X, Y}, RotRad) ->
    rotate({X, Y}, RotRad, {0, 0}).

-spec rotate(vector(), scalar(), vector()) -> vector().
rotate({Xv, Yv}, RotRad, {Xp, Yp}) ->
    % v = vertex/vector, p = point
    NewX = Xp + (Xv - Xp) * math:cos(RotRad) - (Yv - Yp) * math:sin(RotRad),
    NewY = Yp + (Xv - Xp) * math:sin(RotRad) + (Yv - Yp) * math:cos(RotRad),
    {NewX, NewY}.

-spec rotate_polygon([vector()], scalar()) -> [vector()].
rotate_polygon(Vertices, RotRad) ->
    [ow_vector:rotate(Vertex, RotRad) || Vertex <- Vertices].

-spec rotate_polygon([vector()], scalar(), vector()) -> [vector()].
rotate_polygon(Vertices, RotRad, Point) ->
    [ow_vector:rotate(Vertex, RotRad, Point) || Vertex <- Vertices].

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

% This may have duplicate functionality with edges/1
% TODO: Eliminate the extraneous fun
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
    SortDot = lists:sort(Dots),
    [Min | _] = SortDot,
    Max = lists:last(SortDot),
    [Min, Max].

-spec overlap([scalar(), ...], [scalar(), ...]) -> boolean().
overlap(Projection1, Projection2) ->
    Proj1Sort = lists:sort(Projection1),
    Proj2Sort = lists:sort(Projection2),
    [Min1 | _] = Proj1Sort,
    [Min2 | _] = Proj2Sort,
    Max1 = lists:last(Proj1Sort),
    Max2 = lists:last(Proj2Sort),
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

-spec line_of_sight(vector(), vector(), vector(), [vector()]) -> [vector()].
line_of_sight(Location, Upper, Lower, Edges) ->
    % Always check upper and lower rays corresponding to the vision arc.
    Rays = [Upper, Lower],
    % Acceptance function for line segments that fall within the vision arc
    AllVertices = lists:uniq(lists:flatten(Edges)),
    ValidRayCheck =
        fun(Vertex, Acc) ->
            MaybeValidRay = subtract(Location, Vertex),
            case ray_between(MaybeValidRay, Lower, Upper) of
                true ->
                    [MaybeValidRay | Acc];
                false ->
                    Acc
            end
        end,
    % TODO: Doublecheck this. Doesn't seem right on graph paper.
    ValidRays = lists:foldl(ValidRayCheck, Rays, AllVertices),
    % Sort the rays by component
    SortedRays = component_sort(Lower, ValidRays),
    % Check visibility of the segment
    visible_edges(Location, Edges, SortedRays).

visible_edges(Location, Edges, Rays) ->
    visible_edges(Location, Edges, Rays, []).
visible_edges(_Location, _Edges, [], Acc) ->
    lists:uniq(Acc);
visible_edges(Location, Edges, [Ray | Rest], Acc) ->
    % For each ray, cast it through every segment in our list
    % Determine the closest hit, and return that segment.
    Intersections = [
        {
            [EdgeStart, EdgeEnd],
            ray_intersect(Location, Ray, EdgeStart, EdgeEnd)
        }
     || [EdgeStart, EdgeEnd] <- Edges
    ],
    % Filter out the rays that did not hit anything ("false") and
    % create a new list with the segments crossed and distance to each
    FilteredIntersections = [
        {Segment, Distance}
     || {Segment, {true, _Where, Distance}} <- Intersections,
        Intersections /= false
    ],
    % Sort the intersections by the shortest
    case FilteredIntersections of
        [] ->
            visible_edges(Location, Edges, Rest, Acc);
        _ ->
            [{Edge, _Distance} | _Rest] = lists:keysort(
                2, FilteredIntersections
            ),
            % Add the segment to our visibile list
            Acc1 = [Edge | Acc],
            visible_edges(Location, Edges, Rest, Acc1)
    end.

-spec ray_between(vector(), vector(), vector()) -> boolean().
ray_between({RayX, RayY}, {LowerX, LowerY}, {UpperX, UpperY}) ->
    % Dot product of upper rotated ccw by pi/2
    UpperComponent = RayY * UpperX - RayX * UpperY,
    % Dot product of lower rotated cw by pi/2
    % Could be slightly more efficient by bailing out early if you first check
    % to see if !(UpperComponent > ?EPSILON)
    LowerComponent = RayX * LowerY - RayY * LowerX,
    (not (UpperComponent > ?EPSILON)) and (not (LowerComponent > ?EPSILON)).

-spec ray_intersect(vector(), vector(), vector(), vector()) ->
    false | {true, vector(), number()}.
ray_intersect(A, B, C, D) ->
    intersect(A, B, C, D, rayline).

-spec intersect(vector(), vector(), vector(), vector()) -> false | vector().
intersect(A, B, C, D) ->
    intersect(A, B, C, D, lineline).
-spec intersect(
    vector(), vector(), vector(), vector(), rayline | rayray | lineline
) ->
    false
    | {true, vector()}
    | {true, vector(), number()}
    | {true, vector(), {number(), number()}}.
intersect({Ax, Ay} = A, B, {Cx, Cy} = C, D, LineType) ->
    % Let A and B be two points that constitute a line segment.
    % Let C and D be two more points that constitute another line segment.
    R = subtract(A, B),
    {Rx, Ry} = R,
    S = subtract(C, D),
    {Sx, Sy} = S,
    % calculate the 2d 'cross product' of these segments
    case cross(R, S) of
        RcrossS when RcrossS == 0; RcrossS == 0.0 ->
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
                        % TODO: Review. Sometimes lights up too many
                        %       edges.
                        -?EPSILON < U andalso U < 1 + ?EPSILON andalso
                            -?EPSILON < T;
                    %0 =< U andalso U =< 1 andalso 0 =< T;
                    lineline ->
                        0 =< U andalso U =< 1 andalso 0 =< T andalso T =< 1
                end,
            case Intersects of
                true ->
                    case LineType of
                        rayray -> {true, add(A, scale(R, T)), {U, T}};
                        rayline -> {true, add(A, scale(R, T)), T};
                        lineline -> {true, add(A, scale(R, T))}
                    end;
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

%-spec edges([vector()]) -> [vector()].
edges(Vertices) ->
    edges(Vertices, []).
edges([], Acc) ->
    Acc;
edges([First, Second | Rest], Acc) ->
    % Take the first two vertices and make a pair
    Edge = [First, Second],
    % Remove the first vertex and continue
    edges([Second | Rest], First, [Edge | Acc]);
edges([_Last | _Rest], Acc) ->
    % Handle the case of an odd number of edges
    Acc.

%-spec edges([vector()], vector(), [vector()]) -> [[vector()]].
edges([], _First, Acc) ->
    Acc;
edges([A, B | Rest], First, Acc) ->
    Edge = [A, B],
    edges([B | Rest], First, [Edge | Acc]);
edges([Last], First, Acc) ->
    Edge = [Last, First],
    edges([], First, [Edge | Acc]).

% Given a deep list of edges, delete all shared edges, producing only outer
% edges. This trick only works in 2D.
outer_edges(EdgeList) ->
    % There is probably a much more effiient way to do this, but we don't need
    % to do it frequently.
    % For every edge in the list, sort the inner edge lists
    Sorted = [lists:sort(E) || E <- EdgeList],
    % Get the unique vertex pairs
    Uniques = lists:uniq(Sorted),
    % Remove the unique vertex pairs from the edge list to get a list
    % containing only duplicates
    Duplicates = Sorted -- Uniques,
    % Remove the duplicates and their reverses from the unsorted list
    F = fun([X, Y]) ->
        not (lists:member([X, Y], Duplicates) or
            lists:member([Y, X], Duplicates))
    end,
    lists:filter(F, Sorted).

-spec component_sort(vector(), [vector()]) -> [vector()].
component_sort(Lower, Rays) ->
    % from https://basstabs.github.io/2d-line-of-sight/Angle.html
    Fun = fun(A = {Xa, Ya}, B = {Xb, Yb}) ->
        ADotL = dot(A, Lower),
        LHS = abs(ADotL) * ADotL * (Xb * Xb + Yb * Yb),

        BDotL = dot(B, Lower),
        RHS = abs(BDotL) * BDotL * (Xa * Xa + Ya * Ya),
        LHS >= RHS
    end,
    lists:sort(Fun, Rays).

ysort(Vertices) ->
    % https://stackoverflow.com/a/4370294
    % Keysort will only look at selected key, while this will sort the first
    % element properly as well.
    Fun = fun({X1, Y1}, {X2, Y2}) -> {Y1, X1} =< {Y2, X2} end,
    lists:sort(Fun, Vertices).

slope_sort([Start | Rest]) ->
    slope_sort(Start, Rest).
slope_sort({Xs, Ys}, Rest) ->
    Fun = fun({X1, Y1}, {X2, Y2}) ->
        % Avoid divide by zero errors.
        S1D = (X1 - Xs),
        case S1D of
            0 ->
                % infinity =< S2
                false;
            _ ->
                S1 = (Y1 - Ys) / S1D,
                S2D = (X2 - Xs),
                case S2D of
                    0 ->
                        % S1 =< infinity
                        true;
                    _ ->
                        S2 = (Y2 - Ys) / S2D,
                        S1 =< S2
                end
        end
    end,
    [{Xs, Ys} | lists:sort(Fun, Rest)].

% Calculate a 2D convex hull via Graham's Scan technique
-spec convex_hull([vector()]) -> [vector()].
convex_hull(Vertices) ->
    % Sort the list by the lowest (x,y) coordinate relative to the origin, then
    % sort all following points by slope relative to the starting point
    SortedVertices = lists:sort(Vertices),
    SlopedSort = slope_sort(SortedVertices),
    convex_hull(SlopedSort, []).

convex_hull([P1, P2], Acc) ->
    lists:reverse(Acc) ++ [P1, P2];
convex_hull([Start | Rest], []) ->
    convex_hull(Rest, [Start]);
convex_hull(Points, Acc) ->
    % Get the first 3 vertices
    [P1, P2, P3 | Rest] = Points,
    {X1, Y1} = P1,
    {X2, Y2} = P2,
    {X3, Y3} = P3,
    % Calculate the cross product
    % If negative/zero, then the point is point is a cavity or colinear so we
    % discard it.
    P0xP1xP2 = (X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1),
    if
        P0xP1xP2 < 0 ->
            % This angle is bad, discard P3 and try again
            [Last | AccRest] = Acc,
            convex_hull([Last, P1, P3 | Rest], AccRest);
        true ->
            convex_hull([P2, P3 | Rest], [P1 | Acc])
    end.

-spec distance(vector(), vector()) -> pos_integer().
distance({X1, Y1}, {X2, Y2}) ->
    abs(X2 - X1) + abs(Y2 - Y1).
