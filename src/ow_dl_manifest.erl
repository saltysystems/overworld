-module(ow_dl_manifest).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    #{peer := {IP, _Port}} = Req,
    logger:info("Client manifest requested by ~p", [IP]),
    Reply =
        case cowboy_req:parse_qs(Req) of
            [] ->
                % No arguments, just return the manifest
                manifest(Req);
            F ->
                send_file(F, Req)
        end,
    {ok, Reply, State}.

manifest(Req) ->
    % List all registered apps
    Apps = ow_protocol:registered_apps(),
    % Get the list of files
    ProtoFiles = protofiles(Apps),
    logger:info("Protofiles: ~p", [ProtoFiles]),
    % Encode the list via jsone
    Manifest = jsone:encode(ProtoFiles),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"content-disposition">> =>
                <<"attachment; filename=manifest.json">>
        },
        Manifest,
        Req
    ).

send_file([{<<"file">>, F = <<"libow.gd">>}], Req) ->
    ClientAPI = ow_binding:print(),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>,
            <<"content-disposition">> =>
                <<"attachment; filename=", F>>
        },
        ClientAPI,
        Req
    );
send_file([{<<"file">>, BinFile}], Req) ->
    File = binary_to_list(BinFile),
    [App | _] = string:split(File, "."),
    Content = protofile(App),
    case Content of
        error ->
            cowboy_req:reply(404, #{}, Req);
        F ->
            cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => <<"text/plain">>
                },
                F,
                Req
            )
    end.

-spec protofiles(list()) -> [{string(), binary()}, ...].
protofiles(FileList) ->
    protofiles(FileList, []).
protofiles([], Acc) ->
    Acc;
protofiles([H | T], Acc) ->
    Files = list_to_binary(atom_to_list(H) ++ ".proto"),
    protofiles(T, [Files | Acc]).

% return the path of the proto file for the given application
-spec protofile(atom()) -> binary().
protofile(App) ->
    D = code:priv_dir(list_to_existing_atom(App)),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ App ++ ".proto",
    case file:read_file(F) of
        {ok, Proto} -> Proto;
        _ -> error
    end.
