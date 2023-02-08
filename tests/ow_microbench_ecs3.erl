-module(ow_microbench_ecs3).

-export([
    eprof/5,
    fprof/5,
    run/5,
    run_sat/0,
    run_ray/0,
    timer/5,
    check_area_random_test/2,
    check_ray_random_test/2
]).

% This module has some microbenchmarks for various pieces of Overworld

eprof(Count, Area, Depth, Test, World) ->
    QuadTree = ow_collision:new(Area, Depth),
    Entities = generate_random_entities(Count, Area, Area, World),
    TestFun =
        case Test of
            ray -> check_ray_random_test;
            sat -> check_area_random_test
        end,
    {ok, Pid} = eprof:start(),
    {ok, _Result} = eprof:profile(
        [Pid], ow_microbench_ecs3, TestFun, [QuadTree, Entities, World]
    ),
    R = eprof:analyze(),
    eprof:stop(),
    io:format("eprof test results: ~p~n", [R]).

fprof(Count, Area, Depth, Test, World) ->
    QuadTree = ow_collision:new(Area, Depth),
    Entities = generate_random_entities(Count, Area, Area, World),
    case Test of
        sat ->
            fprof:trace(start),
            check_area_random_test(QuadTree, Entities);
        ray ->
            fprof:trace(start),
            check_ray_random_test(QuadTree, Entities)
    end,
    fprof:trace(stop),
    fprof:profile(),
    R = fprof:analyse(),
    io:format("fprof test results: ~p~n", [R]).

timer(Count, Area, Depth, Test, World) ->
    QuadTree = ow_collision:new(Area, Depth),
    Entities = generate_random_entities(Count, Area, Area, World),
    TestFun =
        case Test of
            sat -> check_area_random_test;
            ray -> check_ray_random_test
        end,
    {Time, _Results} = timer:tc(ow_microbench_ecs3, TestFun, [
        QuadTree, Entities
    ]),
    Time.

run_sat() ->
    io:format(
        "SAT: Running 1,000 trials of 1,000 entities in a 10,000 px * 10,000 px arena with quadtree depth of 4~n"
    ),
    run(1000, 1000, 10000, 4, sat).

run_ray() ->
    io:format(
        "RAYCAST: Running 1,000 trials of 1,000 entities in a 10,000 px * 10,000 px arena with quadtree depth of 4~n"
    ),
    run(1000, 1000, 10000, 4, ray).

run(N, Count, Area, Depth, Test) ->
    F = fun(_Elem, AccIn) ->
        World = ow_ecs3:start(),
        R = [timer(Count, Area, Depth, Test, World) | AccIn],
        ow_ecs3:stop(World),
        R
    end,
    Trials = lists:foldl(F, [], lists:seq(1, N)),
    Average = lists:sum(Trials) / N,
    io:format("Average run time: ~p (us)~n", [Average]).

generate_random_entities(Number, XRange, YRange, World) ->
    L = lists:seq(1, Number),
    Bounds = [{-10, -10}, {-10, 10}, {10, -10}, {10, 10}],
    NewEntity =
        fun(_E) ->
            ID = erlang:unique_integer(),
            Phys = #{
                pos => ow_vector:vector_map({
                    rand:uniform(XRange), rand:uniform(YRange)
                }),
                rot => rand:uniform_real() * math:pi()
            },
            ow_ecs3:add_component(phys, Phys, ID, World),
            Hitbox = ow_vector:rect_to_maps(Bounds),
            ow_ecs3:add_component(hitbox, Hitbox, ID, World)
        end,
    lists:foreach(NewEntity, L),
    % Retrieve the entities
    ow_ecs3:match_components([phys, hitbox], World).

check_area_random_test(QuadTree, Entities) ->
    Q1 = QuadTree,
    % Create some entities in the quadtree
    % Function for deriving the position of the entity
    PositionFun = fun({_ID, Components}) ->
        Phys = ow_ecs3:get(phys, Components),
        #{pos := Pos} = Phys,
        ow_vector:vector_tuple(Pos)
    end,
    % Add entity positions to the quadtree
    Q2 = ow_collision:add_entities(Entities, PositionFun, Q1),
    % Calculate an area of interest from one of the entities
    [H | _T] = Entities,
    {_EID, EComponents} = H,
    #{pos := EPos} = ow_ecs3:get(phys, EComponents),
    {POI_X, POI_Y} = ow_vector:vector_tuple(EPos),
    % Extend 100 units in all directions around the entity to define the area
    % to check for collisions
    Left = POI_X - 50,
    Bottom = POI_Y - 50,
    Right = POI_X + 50,
    Top = POI_Y + 50,
    %BoundingBox = fun(#test_entity{bbox = Box, pos = Pos}) ->
    BoundingBox = fun({_ID, Components}) ->
        % translate all coordinates by Pos
        Phys = ow_ecs3:get(phys, Components),
        #{pos := Pos} = Phys,
        Hitbox = ow_ecs3:get(hitbox, Components),
        PosTuple = ow_vector:vector_tuple(Pos),
        BBoxTuple = ow_vector:rect_to_tuples(Hitbox),
        ow_vector:translate(BBoxTuple, PosTuple)
    end,
    %statistics(runtime),
    %statistics(wall_clock),
    Results = ow_collision:check_area(
        {Left, Bottom, Right, Top}, BoundingBox, Q2
    ),
    [
        {Obj1, Obj2}
     || {Obj1, Obj2, Collision} <- Results, Collision == true
    ].

check_ray_random_test(QuadTree, Entities) ->
    Q1 = QuadTree,
    % Function for deriving the position of the entity
    PositionFun = fun({_ID, Components}) ->
        Phys = ow_ecs3:get(phys, Components),
        #{pos := Pos} = Phys,
        ow_vector:vector_tuple(Pos)
    end,
    % Add entity positions to the quadtree
    Q2 = ow_collision:add_entities(Entities, PositionFun, Q1),
    % Create a random ray
    ROx = rand:uniform(10000),
    ROy = rand:uniform(10000),
    RDx = rand:uniform(10),
    RDy = rand:uniform(10),
    % Calculate a random direction for the ray
    % Extend 100 units in all directions around the entity to define the area
    % to check for collisions
    Left = ROx - 50,
    Bottom = ROy - 50,
    Right = ROx + 50,
    Top = ROy + 50,
    BBoxFun = fun({_ID, Components}) ->
        % translate all coordinates by Pos
        Phys = ow_ecs3:get(phys, Components),
        #{pos := Pos, rot := Rot} = Phys,
        Hitbox = ow_ecs3:get(hitbox, Components),
        PosTuple = ow_vector:vector_tuple(Pos),
        BBoxTuple = ow_vector:rect_to_tuples(Hitbox),
        B1 = ow_vector:translate(BBoxTuple, PosTuple),
        ow_vector:rotate_polygon(B1, Rot)
    end,
    % Create a new ray
    ow_collision:check_ray(
        {Left, Bottom, Right, Top}, {ROx, ROy}, {RDx, RDy}, BBoxFun, Q2
    ).

%[statistics(runtime), statistics(wall_clock), Collisions].
