-module(ow_dl_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:info("Got a request for API package from ~p", [IP]),
    % Compile the latest code
    ClientAPI = ow_binding:print(),
    Apps = ow_protocol:registered_apps(),
    ProtoFiles = protofiles(Apps),
    {ok, {"file", Zip}} = zip:create(
        "file", [{"lib_ow.gd", ClientAPI} | ProtoFiles], [memory]
    ),
    logger:info("Successfully generated lib_ow.zip!"),
    Req2 = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/zip">>,
            <<"content-disposition">> =>
                <<"attachment; filename=libow.zip">>
        },
        Zip,
        Req
    ),
    {ok, Req2, State}.

-spec protofiles(list()) -> [{string(), binary()}, ...].
protofiles(FileList) ->
    protofiles(FileList, []).

protofiles([], Acc) ->
    Acc;
protofiles([H | T], Acc) ->
    Files = {atom_to_list(H) ++ ".proto", protofile(H)},
    protofiles(T, [Files | Acc]).

% return the path of the proto file for the given application
-spec protofile(atom()) -> binary().
protofile(App) ->
    D = code:priv_dir(App),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ atom_to_list(App) ++ ".proto",
    {ok, Proto} = file:read_file(F),
    Proto.
