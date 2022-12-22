-module(ow_collision).
-export([
    new/5, new/4, new/3, new/2,
    add_entity/3,
    add_entities/3,
    check_area/3,
    check_ray/5,
    check_ray_test/0
]).

% these aren't eunit tests as such, just some scaffolding toward building those
-export([
    add_entity_test/0,
    add_entities_test/0,
    check_area_test/0
]).

% Generic collision detection module.

%:: {pos_integer(), pos_integer()}
-record(test_entity, {
    pos,
    %:: [{ow_vector:vector(), ...}, ...]
    bbox
}).

-spec new(integer(), integer(), integer(), integer()) ->
    erlquad:erlquad_node().
new(Xmin, Ymin, Xmax, Ymax) ->
    erlquad:new(Xmin, Ymin, Xmax, Ymax, 3).

-spec new(integer(), integer(), integer(), integer(), pos_integer()) ->
    erlquad:erlquad_node().
new(Xmin, Ymin, Xmax, Ymax, Depth) ->
    erlquad:new(Xmin, Ymin, Xmax, Ymax, Depth).

-spec new(pos_integer(), pos_integer()) -> erlquad:erlquad_node().
new(X, Y) ->
    new(X, Y, 3).

-spec new(pos_integer(), pos_integer(), pos_integer()) ->
    erlquad:erlquad_node().
new(X, Y, Depth) ->
    % Random internet stuff suggests the optimal depth of the quadtree should
    % be log4(N) where N is number of entities.
    % log4(25) -> 2.32
    % log4(100) -> 3.32
    % log4(1000) -> 4.98
    erlquad:new(0, 0, X, Y, Depth).

-spec add_entity(any(), fun(), erlquad:erlquad_node()) ->
    erlquad:erlquad_node().
add_entity(Entity, PosFun, Quadtree) ->
    add_entities([Entity], PosFun, Quadtree).

add_entity_test() ->
    Q = new(1000, 1000),
    % coordinates
    Entity = #test_entity{pos = {10, 20}},
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
    add_entity(Entity, PositionFun, Q).

-spec add_entities(list(), fun(), erlquad:erlquad_node()) ->
    erlquad:erlquad_node().
add_entities(Entities, PositionFun, Quadtree) ->
    erlquad:objects_add(Entities, PositionFun, Quadtree).

add_entities_test() ->
    Q = new(1000, 1000),
    Entities = [
        #test_entity{pos = {10, 20}},
        #test_entity{pos = {15, 35}}
    ],
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
    add_entities(Entities, PositionFun, Q).

check_area(
    CheckArea, BoundingBoxFun, Quadtree
) ->
    UniqObjPairs = get_uniques(CheckArea, Quadtree),
    [
        {Obj1, Obj2,
            ow_vector:is_collision(
                BoundingBoxFun(Obj1), BoundingBoxFun(Obj2)
            )}
     || [Obj1, Obj2] <- UniqObjPairs
    ].

check_area_test() ->
    % create a new quadtree
    Q1 = new(10000, 10000),
    % Create some entities in the quadtree
    Entities = [
        #test_entity{
            pos = {10, 20},
            bbox = [{-5, -5}, {-5, 5}, {5, -5}, {5, 5}]
        },
        #test_entity{
            pos = {10, 21},
            bbox = [{-5, -5}, {-5, 5}, {5, -5}, {5, 5}]
        },
        #test_entity{
            pos = {25, 30},
            bbox = [{-5, -5}, {-5, 5}, {5, -5}, {5, 5}]
        }
    ],
    % Function for deriving the position of the entity
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
    % Add entity positions to the quadtree
    Q2 = add_entities(Entities, PositionFun, Q1),
    %io:format("New quadtree is ~p~n", [Q2]),
    % Calculate an area of interest from one of the entities
    [H | _T] = Entities,
    %io:format("Selecting entity to check for collisions: ~p~n", [H]),
    {POI_X, POI_Y} = H#test_entity.pos,
    % Extend 100 units in all directions around the entity to define the area to check for collisions
    Left = POI_X - 50,
    Bottom = POI_Y - 50,
    Right = POI_X + 50,
    Top = POI_Y + 50,
    BoundingBox = fun(#test_entity{bbox = Box, pos = Pos}) ->
        % translate all coordinates by Pos
        ow_vector:translate(Box, Pos)
    end,
    Results = check_area({Left, Bottom, Right, Top}, BoundingBox, Q2),
    [
        {Obj1, Obj2}
     || {Obj1, Obj2, Collision} <- Results, Collision == true
    ].
%io:format("Code time=~p (~p) microseconds~n", [U1, U2]),
%io:format("Collisions at: ~p~n", [Collisions]).

% TODO: It may be desireable to have an early-exit version of this, such that
%       it completes after finding one intersection.
check_ray({L, B, R, T}, RayOrigin, RayDirection, BBoxFun, QuadTree) ->
    % Get the list of objects
    Objects = erlquad:area_query(L, B, R, T, QuadTree),
    % Get all edges
    F = fun(Object, AccIn) ->
        % Get the bounding box vertices in tuple form and convert them
        % to edges
        Edges = ow_vector:edges(BBoxFun(Object)),
        % Add the edges to the accumulator
        Edges ++ AccIn
    end,
    Edges = lists:foldl(F, [], Objects),
    % Test if the ray intersects any of the edges
    TestIntersect =
        fun([P1, P2], Acc) ->
            case ow_vector:ray_intersect(RayOrigin, RayDirection, P1, P2) of
                false ->
                    Acc;
                Result ->
                    [Result | Acc]
            end
        end,
    lists:foldl(TestIntersect, [], Edges).

check_ray_test() ->
    % create a new quadtree
    Q1 = new(1000, 1000, 3),
    % Create some entities in the quadtree
    Entities = [
        #test_entity{
            pos = {10, 10},
            % counter-clockwise winding
            bbox = [{-5, -5}, {-5, 5}, {5, 5}, {5, -5}]
        }#test_entity{
            pos = {10, 21},
            bbox = [{-5, -5}, {-5, 5}, {5, -5}, {5, 5}]
        },
        #test_entity{
            pos = {0, 0},
            bbox = [{-5, -5}, {-5, 5}, {5, -5}, {5, 5}]
        }
    ],
    % Function for deriving the position of the entity
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
    % Add entity positions to the quadtree
    Q2 = add_entities(Entities, PositionFun, Q1),
    % Create a new ray
    {ROx, ROy} = {0, 5},
    RayDirection = {5, 5},
    % Extend 100 units in all directions around the entity to define the area
    % to check for collisions
    Left = ROx - 50,
    Bottom = ROy - 50,
    Right = ROx + 50,
    Top = ROy + 50,
    BBoxFun = fun(#test_entity{bbox = Box, pos = Pos}) ->
        % translate all coordinates by Pos
        ow_vector:translate(Box, Pos)
    end,
    % Create a new ray
    check_ray(
        {Left, Bottom, Right, Top}, {ROx, ROy}, RayDirection, BBoxFun, Q2
    ).

get_uniques({Left, Bottom, Right, Top}, QuadTree) ->
    % Query the area to check
    Entities = erlquad:area_query(
        Left, Bottom, Right, Top, QuadTree
    ),
    %io:format("Entities in the queried area: ~p~n", [Entities]),
    % Create a list of all objects in the area of interest to check.
    ObjPairs = [
        [Obj1, Obj2]
     || Obj1 <- Entities,
        Obj2 <- Entities,
        Obj1 =/= Obj2
    ],
    % Sort the inner list pair, then delete duplicates
    lists:usort([lists:sort(X) || X <- ObjPairs]).
