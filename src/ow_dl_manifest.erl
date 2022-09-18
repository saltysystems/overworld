-module(ow_dl_manifest).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    #{peer := {RawIP, _Port}} = Req,
    IP = inet:ntoa(RawIP),
    Reply =
        case cowboy_req:parse_qs(Req) of
            [] ->
                % No arguments, just return the manifest
                logger:info("Client ~p GET manifest.json", [IP]),
                manifest(Req);
            F ->
                [{<<"file">>, FBin}] = F,
                File = binary_to_list(FBin),
                logger:info("Client ~p GET ~p", [IP, File]),
                send_file(F, Req)
        end,
    {ok, Reply, State}.

manifest(Req) ->
    % List all registered apps
    Apps = ow_protocol:registered_apps(),
    % Get the list of files
    ClientLib =
        case godot_client_lib_version(Req) of
            3 ->
                <<"libow3.gd">>;
            4 ->
                <<"libow4.gd">>
        end,
    ProtoFiles = [ClientLib | protofiles(Apps)],
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

send_file([{<<"file">>, <<"libow3.gd">>}], Req) ->
    ClientAPI = ow_binding:print(3),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>,
            <<"content-disposition">> =>
                <<"attachment; filename=libow3.gd">>
        },
        ClientAPI,
        Req
    );
send_file([{<<"file">>, <<"libow4.gd">>}], Req) ->
    ClientAPI = ow_binding:print(4),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>,
            <<"content-disposition">> =>
                <<"attachment; filename=libow4.gd">>
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
                    <<"content-type">> => <<"text/plain">>,
                    <<"content-disposition">> =>
                        <<<<"attachment; filename=">>/binary,
                            BinFile/binary>>
                },
                F,
                Req
            )
    end.

-spec protofiles(list()) -> [binary(), ...].
protofiles(FileList) ->
    protofiles(FileList, []).
protofiles([], Acc) ->
    Acc;
protofiles([H | T], Acc) ->
    Files = list_to_binary(atom_to_list(H) ++ ".proto"),
    protofiles(T, [Files | Acc]).

% return the path of the proto file for the given application
-spec protofile(string()) -> binary() | error.
protofile(App) ->
    D = code:priv_dir(list_to_existing_atom(App)),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ App ++ ".proto",
    case file:read_file(F) of
        {ok, Proto} -> Proto;
        _ -> error
    end.

-spec godot_client_lib_version(map()) -> 3 | 4.
godot_client_lib_version(#{headers := #{<<"user-agent">> := UserAgent}}) ->
    % TODO: Improve me
    case string:find(UserAgent, <<"GodotEngine/4">>) of
        nomatch ->
            % Assume Godot 3.x
            3;
        _ ->
            4
    end.
