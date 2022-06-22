-module(saline_stats).

-behaviour(cowboy_handler).

-export([init/2]).

-export([poll_status/0]).

init(Req0, State) ->
    GameStatus = poll_status(),
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>
        },
        GameStatus,
        Req0
    ),
    {ok, Req, State}.

poll_status() ->
    Apps = saline_protocol:registered_apps(),
    % Assume every Saline app is an OTP app
    F = fun(Atom) ->
        AString = erlang:atom_to_list(Atom),
        Mod = erlang:list_to_existing_atom(AString ++ "_app"),
        case erlang:function_exported(Mod, status, 0) of
            false ->
                #{};
            _ ->
                Mod:status()
        end
    end,
    StatusMaps = [F(App) || App <- Apps],
    jsone:encode(StatusMaps).
