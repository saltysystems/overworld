%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc ow_client returns the bare minimum client in JSON
%      format, specifying the protobuf schemas and the
%      opcode prefixes
% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ow_client).
-behaviour(cowboy_handler).

-export([init/2, protos/0]).

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, State) ->
    #{peer := {_RawIP, _Port}} = Req,
    Reply = client_json(Req),
    {ok, Reply, State}.

-spec client_json(cowboy_req:req()) -> cowboy_req:req().
client_json(Req) ->
    Client = json:encode(protos()),
    cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"content-disposition">> =>
                <<"attachment; filename=client.json">>
        },
        Client,
        Req
    ).

-spec protos() -> list().
protos() ->
    Protos = ow_protocol:apps(),
    protos(Protos, []).

-spec protos(list(), list()) -> list().
protos([], Acc) ->
    Acc;
protos([{Prefix, #{app := App}} | Rest], Acc) ->
    ProtoDir = code:priv_dir(App),
    ProtoFile = ProtoDir ++ "/proto/" ++ atom_to_list(App) ++ ".proto",
    logger:notice("~p", [ProtoFile]),
    {ok, Schema} = file:read_file(ProtoFile),
    ProtoMap = #{
        app => App,
        schema => Schema,
        prefix => Prefix
    },
    protos(Rest, [ProtoMap | Acc]).
