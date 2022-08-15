-module(ow_db_setup).
-export([prepare_db/0, prepare_db/1]).

-include("../include/db/ow_database.hrl").

-record(ow_table_ids, {table_name, last_id}).

prepare_db() ->
    prepare_db([]).
prepare_db(Path) ->
    E = mnesia:system_info(tables),
    T = tables(),
    F = fun(Table, SoFar) ->
        Member = lists:member(Table, E),
        Member and SoFar
    end,
    case lists:foldl(F, true, T) of
        true ->
            % All of the tables are present, so no op
            ok;
        false ->
            % At least one table doesn't exist, setup the tables
            % Try to get the table path from system config if not defined
            % Then start mnesia up
            case Path of
                [] ->
                    {ok, MDir} = application:get_env(mnesia, dir);
                MDir ->
                    MDir
            end,
            setup(MDir),
            application:start(mnesia)
    end.

tables() ->
    % Update this list
    [ow_table_ids, ow_account].

setup(Path) ->
    case Path of
        [] ->
            io:format("Writing database files to ~p~n", [?DATABASE_DIR]),
            application:set_env(mnesia, dir, ?DATABASE_DIR);
        String ->
            io:format("Writing database files to ~p~n", [String]),
            application:set_env(mnesia, dir, String)
    end,
    application:stop(mnesia),
    install([node()]).

install(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok ->
            rpc:multicall(Nodes, application, start, [mnesia]),
            counter_create_table(Nodes),
            account_create_table(Nodes),
            setup_counters([ow_account]),
            rpc:multicall(Nodes, application, stop, [mnesia]);
        Err ->
            io:format("Couldn't create schema: ~p", [Err]),
            Err
    end.

setup_counters([]) ->
    ok;
setup_counters([H | T]) ->
    Fun = fun() ->
        mnesia:write(
            ow_table_ids,
            #ow_table_ids{table_name = H, last_id = 0},
            write
        )
    end,
    {atomic, ok} = mnesia:transaction(Fun),
    setup_counters(T).

counter_create_table(Nodes) ->
    {_, ok} = mnesia:create_table(ow_table_ids, [
        {record_name, ow_table_ids},
        {attributes, record_info(fields, ow_table_ids)},
        {disc_copies, Nodes}
    ]).

account_create_table(Nodes) ->
    {_, ok} = mnesia:create_table(
        ow_account,
        [
            {attributes, record_info(fields, ow_account)},
            {disc_copies, Nodes}
        ]
    ).
