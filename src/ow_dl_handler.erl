-module(ow_dl_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:info("Got a request for API package from ~p", [IP]),
    % Compile the latest code
    ClientAPI = ow_binding:print(),
    Apps = ow_protocol:apps(),
    ProtoFiles = protofiles(Apps),
    {ok, {"file", Zip}} = zip:create(
        "file", [{"libow.gd", ClientAPI} | ProtoFiles], [memory]
    ),
    logger:info("Successfully generated libow.zip!"),
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

-spec protofiles([{atom(), map()}], list()) -> list().
protofiles([], Acc) ->
    Acc;
protofiles([{_Prefix, #{app := App}} | T], Acc) ->
    Files = {atom_to_list(App) ++ ".proto", protofile(App)},
    protofiles(T, [Files | Acc]).

% return the path of the proto file for the given application
-spec protofile(atom()) -> binary().
protofile(App) ->
    D = code:priv_dir(App),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ atom_to_list(App) ++ ".proto",
    {ok, Proto} = file:read_file(F),
    Proto.
