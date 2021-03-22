-module(goblet_space_player).

-export([new/5]).

-spec new(list(), list(), pos_integer(), list(), list()) -> ok | {error, any()}.
new(Name, ShipName, Appearance, Role, Account) ->
    case
        goblet_util:run_checks([
            fun() -> is_valid_name(Name) end,
            fun() -> is_valid_ship_name(ShipName) end,
            fun() -> is_valid_appearance(Appearance) end,
            fun() -> is_valid_role(Role) end
        ])
    of
        ok ->
            goblet_db:create_player(Name, ShipName, Appearance, Role, Account);
        Error ->
            {error, Error}
    end.

% Internal functions
is_valid_name(Name) when length(Name) < 64 ->
    ok;
is_valid_name(_Name) ->
    too_long.

is_valid_ship_name(ShipName) when length(ShipName) < 64 ->
    ok;
is_valid_ship_name(_ShipName) ->
    too_long.

% should be a positive integer to be a valid protobuf message
is_valid_appearance(Appearance) when Appearance < 10 ->
    ok;
is_valid_appearance(_Appearance) ->
    no_such_appearance.

is_valid_role(Role) ->
    ValidRoles = ["destroyer", "interceptor", "carrier", "command"],
    case lists:member(Role, ValidRoles) of
        true ->
            ok;
        false ->
            not_a_class
    end.
