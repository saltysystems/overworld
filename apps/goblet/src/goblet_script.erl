-module(goblet_script).

-include_lib("luerl/src/luerl.hrl").

-import(luerl_lib, [lua_error/2, badarg_error/3]).

-export([install/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOT READY !                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install(St0) ->
    luerl_heap:alloc_table(table(), St0).

table() ->
    [
        {<<"fac">>, #erl_func{code = fun fac/2}},
        {<<"create_item">>, #erl_func{code = fun create_item/2}}
    ].

fac(As, St) ->
    case get_number_args(As) of
        [N | _] when is_number(N) -> {[fac(N)], St};
        _ -> badarg_error(fac, As, St)
    end.
fac(0) -> 1;
fac(N) -> N * fac(N - 1).

create_item(As, St) ->
    {[As], St}.
% Name, ActionPoints, Action, TargetType, TargetDamage, TargetHealth, StatusEffect, Price
%{goblet_db:create_item(N, AP, Act, TT, TD, TH, SE, Price), St}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_number_args(As) ->
    lists:map(fun luerl_lib:arg_to_number/1, As).

% put me in another file, probably
%run() ->
%    St0 = luerl:init(),
%    St1 = load_lib(<<"goblet_script">>,goblet_script,St0),
%
%    % Calculate the factorial of 10
%    {[Return], St2} = luerl:do("return goblet_script.fac(10)", St1),
%    {[Return2], St3} = luerl:do("return goblet_script.create_item(\"foo\", 12)", St2),
%    io:format("fac(10) = ~p; create_item() = ~p", [Return, Return2]).
%
%load_lib(Key, Mod, St0) ->
%    {Tab,St1} = Mod:install(St0),
%    %% Add key to global and to package.loaded.
%    St2 = luerl_emul:set_global_key(Key, Tab, St1),
%    luerl_emul:set_table_keys([<<"package">>,<<"loaded">>,Key], Tab, St2).
