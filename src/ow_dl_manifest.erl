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
    Apps = ow_protocol:apps(),
    % Get the list of files

    % only support 4
    ClientLibs = [<<"libow4.gd">>],
    ProtoFiles = ClientLibs ++ protofiles(Apps),
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

send_file([{<<"file">>, <<"WebSocketClient.gd">>}], Req) ->
    PrivDir = code:priv_dir(overworld),
    {ok, WSClient} = file:read_file(PrivDir ++ "static/WebSocketClient.gd"),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>,
            <<"content-disposition">> =>
                <<"attachment; filename=WebSocketClient.gd">>
        },
        WSClient,
        Req
    );
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
protofiles([{_Prefix, {AppName, {_Mod, _Fun}}} | T], Acc) ->
    % Simple namer
    ProtoName = atom_to_list(AppName),
    Files = list_to_binary(ProtoName ++ ".proto"),
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
