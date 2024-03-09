-module(ow_dl_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
%% @doc Initializes the handler and handles the request for the API package.
%% Compiles the latest code, generates the client API and protocol files,
%% creates a ZIP archive containing the files, and sends it as the response.
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
%% @doc Converts a list of application information into a list of protocol files.
protofiles(FileList) ->
    protofiles(FileList, []).

-spec protofiles([{atom(), map()}], list()) -> list().
%% @doc Helper function to recursively process the list of application information
%% and build the list of protocol files.
protofiles([], Acc) ->
    Acc;
protofiles([{_Prefix, #{app := App}} | T], Acc) ->
    Files = {atom_to_list(App) ++ ".proto", protofile(App)},
    protofiles(T, [Files | Acc]).

-spec protofile(atom()) -> binary().
%% @doc Retrieves the protocol file for the given application.
protofile(App) ->
    D = code:priv_dir(App),
    % Very hard-coded and rudimentary.
    F = D ++ "/proto/" ++ atom_to_list(App) ++ ".proto",
    {ok, Proto} = file:read_file(F),
    Proto.
