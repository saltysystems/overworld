-module(goblet_api).

-export([setup_api/0]).

funs() ->
    [
        {"goblet.ship_component_new", fun(
            [Name, Index, Type, Appearance, Attributes]
        ) ->
            goblet_ship_component:new(
                Name,
                Index,
                Type,
                Appearance,
                Attributes
            )
        end},
        {"goblet.ship_component_delete", fun([ID]) ->
            goblet_ship_component:delete(ID)
        end},
        {"goblet.ship_component_get", fun([ID]) ->
            Obj = goblet_ship_component:get(ID),
            goblet_ship_component:to_map(Obj)
        end}
    ].

setup_api() ->
    setup_api(funs()).

setup_api([]) ->
    ok;
setup_api([{Name, Fun} | T]) ->
    gdminus_int:insert_function(Name, Fun),
    setup_api(T).
