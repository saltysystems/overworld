-module(ow_microbench).

-export([
    eprof/4,
    fprof/4,
    run/5,
    run_sat/0,
    run_ray/0,
    timer/4,
    check_area_random_test/3,
    check_ray_random_test/3
]).

% This module has some microbenchmarks for various pieces of Overworld

%:: {pos_integer(), pos_integer()}
-record(test_entity, {
    pos,
    rot,
    %:: [{ow_vector:vector(), ...}, ...]
    bbox
}).

eprof(Count, Area, Depth, Test) ->
    TestFun = case Test of
                  ray -> check_ray_random_test;
                  sat -> check_area_random_test
              end,
    {ok, Pid} = eprof:start(),
    {ok, _Result} = eprof:profile(
        [Pid], ow_microbench, TestFun, [Count, Area, Depth]
    ),
    R = eprof:analyze(),
    eprof:stop(),
    io:format("eprof test results: ~p~n", [R]).

fprof(Count, Area, Depth, Test) ->
    case Test of
        sat ->
            fprof:trace(start),
            check_area_random_test(Count, Area, Depth);
        ray ->
            fprof:trace(start),
            check_ray_random_test(Count, Area, Depth)
    end,
    fprof:trace(stop),
    fprof:profile(),
    R = fprof:analyse(),
    io:format("fprof test results: ~p~n", [R]).

timer(Count, Area, Depth, Test) ->
    TestFun =
        case Test of
            sat -> check_area_random_test;
            ray -> check_ray_random_test
        end,
    {Time, _Results} = timer:tc(ow_microbench, TestFun, [
        Count, Area, Depth
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
        [timer(Count, Area, Depth, Test) | AccIn]
    end,
    Trials = lists:foldl(F, [], lists:seq(1, N)),
    Average = lists:sum(Trials) / N,
    io:format("Average run time: ~p (us)~n", [Average]).

generate_random_entities(Number, XRange, YRange) ->
    L = lists:seq(1, Number),
    Bounds = [{-10, -10}, {-10, 10}, {10, -10}, {10, 10}],
    [
        #test_entity{
            pos = {rand:uniform(XRange), rand:uniform(YRange)},
            rot = rand:uniform_real() * math:pi(),
            bbox = Bounds
        }
     || _N <- L
    ].

check_area_random_test(Count, Area, Depth) ->
    % create a new quadtree
    Q1 = ow_collision:new(Area, Depth),
    % Create some entities in the quadtree
    Entities = generate_random_entities(Count, Area, Area),
    % Function for deriving the position of the entity
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
    % Add entity positions to the quadtree
    Q2 = ow_collision:add_entities(Entities, PositionFun, Q1),
    % Calculate an area of interest from one of the entities
    [H | _T] = Entities,
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
    %statistics(runtime),
    %statistics(wall_clock),
    Results = ow_collision:check_area(
        {Left, Bottom, Right, Top}, BoundingBox, Q2
    ),
    [
        {Obj1, Obj2}
     || {Obj1, Obj2, Collision} <- Results, Collision == true
    ].

check_ray_random_test(Count, Area, Depth) ->
    Q1 = ow_collision:new(Area, Depth),
    % Create some entities in the quadtree
    Entities = generate_random_entities(Count, Area, Area),
    % Function for deriving the position of the entity
    PositionFun = fun(#test_entity{pos = {X, Y}}) -> {X, Y} end,
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
    BBoxFun = fun(#test_entity{bbox = Box, pos = Pos, rot = Rot}) ->
        % translate all coordinates by Pos
        B1 = ow_vector:translate(Box, Pos),
        ow_vector:rotate_polygon(B1, Rot)
    end,
    % Create a new ray
    ow_collision:check_ray(
        {Left, Bottom, Right, Top}, {ROx, ROy}, {RDx, RDy}, BBoxFun, Q2
    ).

%[statistics(runtime), statistics(wall_clock), Collisions].
