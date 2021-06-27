-module(goblet_util).
-export([
    run_checks/1,
    run_bchecks/1,
    any_in_list/2,
    pipeline/2
]).

-spec run_bchecks(list()) -> ok | any().
run_bchecks([]) ->
    ok;
run_bchecks([H | T]) when is_function(H, 0) ->
    case H() of
        true -> run_checks(T);
        _ -> false
    end.

-spec run_checks(list()) -> ok | any().
run_checks([]) ->
    ok;
run_checks([H | T]) when is_function(H, 0) ->
    case H() of
        ok -> run_checks(T);
        Error -> Error
    end.

% poor man's pipe
-spec pipeline(any(), list()) -> any().
pipeline(Input, Funs) ->
    lists:foldl(fun(F, State) -> F(State) end, Input, Funs).

any_in_list(L1, L2) ->
    % Checks to see if any item in L1 is present in L2
    [X || X <- L1, lists:member(X, L2) == true].
