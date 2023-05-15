-module(ow_stats).

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
    Apps = ow_protocol:apps(),
    % Assume every Overworld app is an OTP app
    F = fun({AppName, {Mod, _Fun}}) ->
        AString = erlang:atom_to_list(AppName),
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
