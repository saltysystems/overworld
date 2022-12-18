-module(ow_microbench).

-export([eprof/3, fprof/3, run/4, run/0, timer/3, check_area_random_test/3]).

% This module has some microbenchmarks for various pieces of Overworld

%:: {pos_integer(), pos_integer()}
-record(test_entity, {
    pos,
    %:: [{ow_vector:vector(), ...}, ...]
    bbox
}).

eprof(Count, Area, Depth) ->
    {ok, Pid} = eprof:start(),
    {ok, _Result} = eprof:profile(
        [Pid], ow_microbench, check_area_random_test, [Count, Area, Depth]
    ),
    R = eprof:analyze(),
    eprof:stop(),
    io:format("eprof test results: ~p~n", [R]).

fprof(Count, Area, Depth) ->
    fprof:trace(start),
    check_area_random_test(Count, Area, Depth),
    fprof:trace(stop),
    fprof:profile(),
    R = fprof:analyse(),
    io:format("fprof test results: ~p~n", [R]).

timer(Count, Area, Depth) ->
    {Time, _Results} = timer:tc(ow_microbench, check_area_random_test, [
        Count, Area, Depth
    ]),
    Time.

run() ->
    io:format(
        "Running 1,000 trials of 1,000 entities in a 10,000 px * 10,000 px arena with quadtree depth of 3"
    ),
    run(1000, 1000, 10000, 3).
run(N, Count, Area, Depth) ->
    F = fun(_Elem, AccIn) ->
        [timer(Count, Area, Depth) | AccIn]
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
            bbox = Bounds
        }
     || _N <- L
    ].

check_area_random_test(Count, Area, Depth) ->
    % create a new quadtree
    Q1 = ow_collision:new(Area, Area, Depth),
    % Create some entities in the quadtree
    % Entities = [
    %           #test_entity{
    %              pos={10,20},
    %              bbox=[{-5,-5}, {-5,5}, {5,-5}, {5,5}]
    %              },
    %           #test_entity{
    %              pos={10,21},
    %              bbox=[{-5,-5}, {-5,5}, {5,-5}, {5,5}]
    %             },
    %           #test_entity{
    %              pos={25,30},
    %              bbox=[{-5,-5}, {-5,5}, {5,-5}, {5,5}]
    %             }
    %          ],
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
%[statistics(runtime), statistics(wall_clock), Collisions].
