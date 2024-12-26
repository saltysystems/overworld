-module(ow_client_lib).
-behaviour(cowboy_handler).

-export([init/2]).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
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
    % Static files
    ClientLibs = [<<"libow4.gd">>],
    % Protobuf files
    ProtoFiles = protofiles(ow_protocol:apps()),
    % Encode the list via jsone
    FileList = lists:flatten([ProtoFiles | ClientLibs]),
    Manifest = jsone:encode(FileList),
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
send_file([{<<"file">>, <<"libow4.gd">>}], Req) ->
    ClientAPI = ow_binding:print(),
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
    % Now split the path into constituent components
    Content = sanitized_protofile(File),
    case Content of
        {error, _} ->
            cowboy_req:reply(404, #{}, Req);
        {ok, F} ->
            cowboy_req:reply(
                200,
                #{
                    <<"content-type">> => <<"text/plain">>,
                    <<"content-disposition">> =>
                        <<<<"attachment; filename=">>/binary, BinFile/binary>>
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
protofiles([{_Prefix, #{app := App}} | T], Acc) ->
    % Lookup all proto files in priv dir
    ProtoDir = code:priv_dir(App) ++ "/proto",
    case file:list_dir(ProtoDir) of
        {ok, FileList} ->
            F1 = lists:map(
                fun(Elem) ->
                    list_to_binary(filename:flatten([App, "/", Elem]))
                end,
                FileList
            ),
            logger:debug("F1: ~p", [F1]),
            protofiles(T, [F1 | Acc]);
        {error, _E} ->
            protofiles(T, Acc)
    end.

-spec sanitized_protofile(string()) -> {ok, binary()} | {error, atom()}.
sanitized_protofile(Input) ->
    [App, Protofile] = filename:split(Input),
    % Only accept and return protobuf files or crash
    ".proto" = filename:extension(Protofile),
    % Get the protofile
    D = code:priv_dir(list_to_existing_atom(App)),
    F = D ++ "/proto/" ++ Protofile,
    file:read_file(F).
