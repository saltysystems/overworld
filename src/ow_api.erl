-module(ow_api).

-export([setup_api/0]).

% EXAMPLE
funs() ->
    [
        {"ow.pow", fun([X, Y]) -> math:pow(X, Y) end}
    ].

setup_api() ->
    setup_api(funs()).

setup_api([]) ->
    ok;
setup_api([{Name, Fun} | T]) ->
    gdminus_int:insert_function(Name, Fun),
    setup_api(T).
