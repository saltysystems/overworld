-module(goblet_util).
-export([run_checks/1]).

-spec run_checks(list()) -> ok | any().
run_checks([]) ->
    ok;
run_checks([H | T]) when is_function(H, 0) ->
    case H() of
        ok -> run_checks(T);
        Error -> Error
    end.
