-module(gremlin_dl_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req1, State) ->
    % Compile the latest code
    ClientAPI = gremlin_binding:print(),
    Apps = gremlin_protocol:registered_apps(),
    ProtoFiles = protofiles(Apps),
    {ok, {"file", Zip}} = zip:create("file", [{"libgremlin.gd", ClientAPI} | ProtoFiles], [memory]),
    Req2 = cowboy_req:reply(
             200,
             #{
               <<"content-type">> => <<"application/zip">>,
               <<"content-disposition">> => <<"attachment; filename=libgremlin.zip">>
              },
             Zip,
             Req1
            ),
    {ok, Req2, State}.

-spec protofiles(list()) -> [{string(), string()}, ...].
protofiles(FileList) -> 
    protofiles(FileList, []).

protofiles([], Acc) ->
    Acc;
protofiles([H|T], Acc) ->
    Files = {atom_to_list(H) ++ ".proto", protofile(H)},
    protofiles(T, [Files|Acc]).


% return the path of the proto file for the given application
-spec protofile(atom()) -> list().
protofile(App) ->
    D = code:priv_dir(App),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ atom_to_list(App) ++ ".proto",
    {ok, Proto} = file:read_file(F),
    Proto.
