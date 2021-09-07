-module(goblet_grid).

-export([new/1, new/2, new/3, get_col/2, put_col/3, get_row/2, put_row/3, put/3, get/2]).

-type coords() :: {pos_integer(), pos_integer()}.

% Create a square grid of size Size.
-spec new(pos_integer()) -> map().
new(Size) ->
    new(Size,Size, {}).

-spec new(pos_integer(), any()) -> map().
new(Size, InitialVal) ->
    new(Size, Size, InitialVal).

-spec new(pos_integer(), pos_integer(), any()) -> map().
new(N, M, InitialVal) ->
    Coords = [{R, C} || R <- lists:seq(1,M), C <- lists:seq(1,N)],
    lists:foldl(fun(X, Map) -> maps:put(X,InitialVal,Map) end, maps:new(), Coords).

-spec get(coords(), map()) -> map().
get({X,Y}, Map) -> 
    maps:get({X,Y}, Map).

-spec get_col(pos_integer(), map()) -> map().
get_col(Col, Map) ->
    Pred = fun({_R,C},_V) -> C == Col end,
    maps:filter(Pred, Map).

-spec get_row(pos_integer(), map()) -> any().
get_row(Row, Map) ->
    Pred = fun({R,_C},_V) -> R == Row end,
    maps:filter(Pred, Map).

-spec put(coords(), any(), map()) -> map().
put({X,Y}, Val, Map) ->
    maps:put({X,Y}, Val, Map).

-spec put_col(pos_integer(), any(), map()) -> map().
put_col(Col, NewVal, Map) ->
    Fun = fun({_R, C},_V) when C == Col -> {true, NewVal}; 
             (_,_) -> false 
          end,
    Filter = maps:filtermap(Fun, Map),
    maps:merge(Map, Filter).

-spec put_row(pos_integer(), any(), map()) -> map().
put_row(Row, NewVal, Map) ->
    Fun = fun({R, _C},_V) when R == Row -> {true, NewVal}; 
             (_,_) -> false 
          end,
    Filter = maps:filtermap(Fun, Map),
    maps:merge(Map, Filter).
