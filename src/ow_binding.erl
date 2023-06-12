-module(ow_binding).

% This module generates GDScript code to handle game-specific signaling.  The
% module in its 3rd major overhaul and shows no signs of being done.  I have no
% doubt there will be even more to do with Godot 4.0

-export([
    write/0,
    pb_to_godot_type/1,
    print/1,
    print/0,
    get_encoders/0,
    load_scripts/1,
    generate_prefixes/0,
    generate_signals/0,
    generate_enums/1,
    generate_submsgs/1,
    generate_unmarshall/0,
    generate_marshall_submsgs/1,
    generate_marshall/0,
    generate_router/0
]).

-include_lib("eunit/include/eunit.hrl").

-define(TAB, [9]).
-define(TAB(N),
    lists:foldl(fun(_N, Acc0) -> [9] ++ Acc0 end, [], lists:seq(0, N - 1))
).
-define(DEFAULT_ENCODER, overworld_pb).
-define(DEFAULT_TEMPLATE_3, "templates/libow3.mustache").
-define(DEFAULT_TEMPLATE_4, "templates/libow4.mustache").

write() ->
    file:write_file(
        "priv/static/libow.gd", ow_binding:print(4)
    ).

print() ->
    print(4).
print(Version) ->
    Encoders = get_encoders(),
    Preloads = load_scripts(Encoders),
    Enums = generate_enums(Encoders),
    Signals = generate_signals(),
    Prefixes = generate_prefixes(),
    Router = generate_router(),
    Submsgs = [generate_submsgs(E) || E <- Encoders],
    Unmarshall = generate_unmarshall(),
    MarshallSubmsgs = [generate_marshall_submsgs(E) || E <- Encoders],
    Marshall = generate_marshall(),
    Map = #{
        "preloads" => Preloads,
        "constants" => Enums,
        "signals" => Signals,
        "prefixes" => Prefixes,
        "router" => Router,
        "submsgs" => Submsgs,
        "unmarshall" => Unmarshall,
        "marshall_submsgs" => MarshallSubmsgs,
        "marshall" => Marshall
    },
    T = get_template(Version),
    bbmustache:compile(T, Map).

get_template(Version) ->
    PrivDir = code:priv_dir(overworld),
    Template =
        case Version of
            3 -> ?DEFAULT_TEMPLATE_3;
            4 -> ?DEFAULT_TEMPLATE_4
        end,
    bbmustache:parse_file(
        PrivDir ++ "/" ++ Template
    ).

pb_to_godot_type(Type) ->
    % https://docs.godotengine.org/en/latest/tutorials/scripting/gdscript/gdscript_basics.html
    % All ints are internally handled as int64_t in GDScript (2.0)
    case Type of
        double -> float;
        float -> float;
        int32 -> int;
        int64 -> int;
        uint32 -> int;
        uint64 -> int;
        sint32 -> int;
        sint64 -> int;
        fixed32 -> int;
        fixed64 -> int;
        sfixed32 -> int;
        sfixed64 -> int;
        bool -> bool;
        string -> 'String';
        bytes -> 'PackedByteArray';
        % Do the best you can.
        _Other -> void
    end.

-spec get_encoders() -> list().
get_encoders() ->
    % For each RPC, build a list of encoders
    G = fun(Type) ->
        RPCs = ow_protocol:rpcs(Type),
        F = fun(RPC, Acc) ->
            #{encoder := Encoder} = ow_protocol:rpc(RPC, Type),
            [Encoder | Acc]
        end,
        lists:foldl(F, [], RPCs)
    end,
    ClientEncoders = G(client),
    ServerEncoders = G(server),
    % Uniq the list to get the actual encoders used.
    lists:uniq(ClientEncoders ++ ServerEncoders).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate messages and submsgs                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Unpacking the overworld uber object example
% func unpack_overworld(object):
%	if object.has_session_beacon():
%		print("has session beacon")
%		return unpack_session_beacon(object.get_session_beacon())
%	elif object.has_gen_response():
%		print("has gen response")
%	elif object.has_account_new():
%		print("has account new")

-spec filter_for_pure_msgs(map()) -> list().
filter_for_pure_msgs(Encoder) ->
    #{lib := EncoderLib} = Encoder,
    MsgList = erlang:apply(EncoderLib, get_msg_defs, []),
    filter_for_pure_msgs(MsgList, []).
filter_for_pure_msgs([], Acc) ->
    Acc;
filter_for_pure_msgs([{{enum, _}, _} | Rest], Acc) ->
    % Skip enums (for now?)
    filter_for_pure_msgs(Rest, Acc);
filter_for_pure_msgs([{{msg, Name}, MapList} | Rest], Acc) ->
    Predicate = fun(Map) ->
        T = maps:get(type, Map, undefined),
        case T of
            {msg, _} -> false;
            undefined -> false;
            _ -> true
        end
    end,
    FilteredList = lists:filter(Predicate, MapList),
    % If the lists match, then the message was impure and we should skip it.
    Acc1 =
        case FilteredList of
            MapList -> [Name | Acc];
            _ -> Acc
        end,
    filter_for_pure_msgs(Rest, Acc1).

-spec generate_submsgs(map()) -> string().
generate_submsgs(Encoder) ->
    % Filter for pure messages in this encoder
    Pures = filter_for_pure_msgs(Encoder),
    St0 = generate_impure_submsgs(Encoder),
    % For every pure, create a function snippet
    St1 = generate_pure_submsgs(Encoder, Pures, []),
    St0 ++ St1.

generate_pure_submsgs(_Encoder, [], Acc) ->
    Acc;
generate_pure_submsgs(Encoder, [vector2 | Rest], Acc) ->
    % A bit of a cheat for vector2s and other special Godot types that are
    % not understood by Protobuf
    Signature = "func unpack_vector2(object):\n",
    Body =
        ?TAB ++ "if typeof(object) == TYPE_ARRAY and object != []:\n" ++
            ?TAB(2) ++ "var array = []\n" ++
            ?TAB(2) ++ "for obj in object:\n" ++
            ?TAB(3) ++ "var vec = Vector2(obj.get_x(), obj.get_y())\n" ++
            ?TAB(3) ++ "array.append(vec)\n" ++
            ?TAB(2) ++ "return array\n" ++
            ?TAB ++ "elif typeof(object) == TYPE_ARRAY and object == []:\n" ++
            ?TAB(2) ++ "return []\n" ++
            ?TAB ++ "else:\n" ++
            ?TAB(2) ++ "var vec = Vector2(object.get_x(), object.get_y())\n" ++
            ?TAB(2) ++ "return vec\n",
    generate_pure_submsgs(Encoder, Rest, [Signature ++ Body | Acc]);
generate_pure_submsgs(Encoder, [MessageName | Rest], Acc) ->
    #{lib := EncoderLib} = Encoder,
    Defn = erlang:apply(EncoderLib, fetch_msg_def, [MessageName]),
    Signature =
        "func unpack_" ++ fix_delim(atom_to_list(MessageName)) ++
            "(object):\n",
    Body =
        ?TAB ++ "if typeof(object) == TYPE_ARRAY and object != []:\n" ++
            ?TAB(2) ++ "var array = []\n" ++
            ?TAB(2) ++ "for obj in object:\n" ++
            generate_submsg_body(Defn, "obj", 3, []) ++
            ?TAB(3) ++ generate_submsg_dict(Defn) ++
            ?TAB(3) ++ "array.append(dict)\n" ++
            ?TAB(2) ++ "return array\n" ++
            ?TAB ++ "elif typeof(object) == TYPE_ARRAY and object == []:\n" ++
            ?TAB(2) ++ "return []\n" ++
            ?TAB ++ "else:\n" ++
            generate_submsg_body(Defn, "object", 2, []) ++
            ?TAB(2) ++ generate_submsg_dict(Defn) ++
            ?TAB(2) ++ "return dict\n",
    generate_pure_submsgs(Encoder, Rest, Signature ++ Body ++ Acc).

generate_submsg_body([], _Prefix, _TabLevel, Acc) ->
    Acc;
generate_submsg_body(
    [Defn = #{type := {msg, Submsg}} | T], Prefix, TabLevel, Acc
) ->
    % Impure submsg does not have well known types and we need to call one of
    % the lower-level functions to deal with it
    Name = atom_to_list(maps:get(name, Defn)),
    Type = fix_delim(atom_to_list(Submsg)),
    Body =
        ?TAB(TabLevel) ++ "var " ++ Name ++ " = unpack_" ++ Type ++ "(" ++
            Prefix ++ ".get_" ++ Name ++ "())\n",
    generate_submsg_body(T, Prefix, TabLevel, Body ++ Acc);
generate_submsg_body([H | T], Prefix, TabLevel, Acc) ->
    % All members of a pure submsg are of well-known types
    Name = atom_to_list(maps:get(name, H)),
    % Godobuf should automagically generate arrays as appropriate for
    % well-knowns, so no need to special case these.
    Body =
        ?TAB(TabLevel) ++ "var " ++ Name ++ " = " ++ Prefix ++ ".get_" ++
            Name ++
            "()\n",
    generate_submsg_body(T, Prefix, TabLevel, Body ++ Acc).

generate_submsg_dict(Definitions) ->
    Pre = "var dict = {",
    generate_submsg_dict(Definitions, Pre).
generate_submsg_dict([], Acc) ->
    Acc ++ "}\n";
generate_submsg_dict([H | T], Acc) ->
    Name = atom_to_list(maps:get(name, H)),
    Pair = "'" ++ Name ++ "': " ++ Name ++ ", ",
    generate_submsg_dict(T, Acc ++ Pair).

generate_impure_submsgs(Encoder) ->
    Pures = filter_for_pure_msgs(Encoder),
    #{lib := EncoderLib} = Encoder,
    AllMessages = erlang:apply(EncoderLib, get_msg_names, []),
    Impures = AllMessages -- Pures,
    generate_impure_submsgs(Encoder, Impures, []).
generate_impure_submsgs(_Encoder, [], Acc) ->
    Acc;
generate_impure_submsgs(Encoder, [H | T], Acc) ->
    #{lib := EncoderLib} = Encoder,
    Defn = erlang:apply(EncoderLib, fetch_msg_def, [H]),
    % Definition is a list
    [Inner | _Rest] = Defn,
    Signature =
        "func unpack_" ++ fix_delim(atom_to_list(H)) ++ "(object):\n",
    Body =
        case maps:get(fields, Inner, undefined) of
            undefined ->
                impure_submsg_body(Defn);
            Fields ->
                oneof_body(Fields, EncoderLib) ++ "\n"
        end,
    generate_impure_submsgs(Encoder, T, Signature ++ Body ++ Acc).

impure_submsg_body(Defn) ->
    ?TAB ++ "if typeof(object) == TYPE_ARRAY and object != []:\n" ++
        ?TAB(2) ++ "var array = []\n" ++
        ?TAB(2) ++ "for obj in object:\n" ++
        generate_submsg_body(Defn, "obj", 3, []) ++
        ?TAB(3) ++ generate_submsg_dict(Defn) ++
        ?TAB(3) ++ "array.append(dict)\n" ++
        ?TAB(2) ++ "return array\n" ++
        ?TAB ++ "elif typeof(object) == TYPE_ARRAY and object == []:\n" ++
        ?TAB(2) ++ "return []\n" ++
        ?TAB ++ "else:\n" ++
        generate_submsg_body(Defn, "object", 2, []) ++
        ?TAB(2) ++ generate_submsg_dict(Defn) ++
        ?TAB(2) ++ "return dict\n".

% Unpacking the overworld uber object example
% func unpack_overworld(object):
%	if object.has_session_beacon():
%		print("has session beacon")
%		return unpack_session_beacon(object.get_session_beacon())
%	elif object.has_gen_response():
%		print("has gen response")
%	elif object.has_account_new():
%		print("has account new")

% TODO: doesn't handle the case of multiple oneofs
oneof_body(Fields, EncoderLib) ->
    F =
        fun
            (Map, []) ->
                % Handle the first case separately
                #{name := Name} = Map,
                Body =
                    ?TAB ++ "if object.has_" ++ atom_to_list(Name) ++
                        "():\n" ++ ?TAB(2) ++ "var d = unpack_" ++
                        atom_to_list(Name) ++ "(object.get_" ++
                        atom_to_list(Name) ++ "())\n" ++
                        emit_signal(Name, EncoderLib),
                [Body];
            (Map, Acc) ->
                #{name := Name} = Map,
                Body =
                    ?TAB ++ "elif object.has_" ++ atom_to_list(Name) ++
                        "():\n" ++ ?TAB(2) ++ "var d = unpack_" ++
                        atom_to_list(Name) ++ "(object.get_" ++
                        atom_to_list(Name) ++ "())\n" ++
                        emit_signal(Name, EncoderLib),
                [Body | Acc]
        end,
    [lists:flatten(lists:reverse(lists:foldl(F, [], Fields)))].

emit_signal(ProtoMsg, EncoderLib) ->
    ProtoMsgStr = atom_to_list(ProtoMsg),
    ?TAB(2) ++ "emit_signal('server_" ++ ProtoMsgStr ++ "'," ++
        dict_fields_to_str(field_info({EncoderLib, ProtoMsg})) ++
        "\)\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate enums                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_enums(Encoder) ->
    generate_enums(Encoder, []).
generate_enums([], Acc) ->
    [Acc];
generate_enums([Encoder | Rest], Acc) ->
    #{lib := EncoderLib} = Encoder,
    Enums = erlang:apply(EncoderLib, get_enum_names, []),
    % Process all enums
    Comment = "# via " ++ atom_to_list(EncoderLib) ++ "\n",
    Acc1 = lists:flatten([
        Acc | [Comment | stringify_enums(EncoderLib, Enums)]
    ]),
    generate_enums(Rest, Acc1).

stringify_enums(EncoderLib, Enums) ->
    stringify_enums(EncoderLib, Enums, []).
stringify_enums(_EncoderLib, [], Acc) ->
    [Acc];
stringify_enums(EncoderLib, [H | T], Acc) ->
    % E has the structure [{atom(), non_negative_integer()}, ...]
    EnumName = lists:flatten(string:replace(atom_to_list(H), ".", "_")),
    %logger:notice("EnumName: ~p", [EnumName]),
    EncStr = string:titlecase(atom_to_list(EncoderLib)),
    %logger:notice("EncStr: ~p", [EncStr]),
    Prefix = "enum " ++ EnumName ++ " {\n",
    %logger:notice("Prefix: ~p", [Prefix]),
    E = erlang:apply(EncoderLib, fetch_enum_def, [H]),
    Estr = [
        ?TAB ++ atom_to_list(Name) ++ " = " ++ EncStr ++ "." ++
            atom_to_list(H) ++ "." ++ atom_to_list(Name) ++ ",\n"
     || {Name, _Value} <- E
    ],
    %logger:notice("Estr: ~p", [Estr]),
    Acc1 = Acc ++ Prefix ++ lists:flatten(Estr) ++ "}\n",
    %logger:notice("Acc1: ~p", [Acc1]),
    stringify_enums(EncoderLib, T, Acc1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Preload Protobuf scripts                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_scripts(Encoders) ->
    load_scripts(Encoders, []).
load_scripts([], Acc) ->
    Acc;
load_scripts([Encoder | Rest], Acc) ->
    #{lib := EncoderLib} = Encoder,
    Const =
        "const " ++ string:titlecase(atom_to_list(EncoderLib)) ++
            " = preload('",
    Script = atom_to_list(EncoderLib) ++ ".gd')",
    load_scripts(Rest, [Const ++ Script | Acc]).

load_scripts_test() ->
    E = ['overworld_pb', 'game_pb'],
    Results = load_scripts(E),
    [GameResult, OverworldResult | _Rest] = Results,
    OverworldTest = "const Overworld_pb = preload('overworld_pb.gd')",
    ?assertEqual(OverworldTest, OverworldResult),
    GameTest = "const Game_pb = preload('game_pb.gd')",
    ?assertEqual(GameTest, GameResult).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup Godot signaling                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_signals() ->
    Type = client,
    RPCs = ow_protocol:rpcs(Type),
    F = fun(RPC, Acc) ->
        #{encoder := Encoder} = ow_protocol:rpc(RPC, Type),
        #{lib := EncoderLib} = Encoder,
        [next_signal(RPC, EncoderLib) | Acc]
    end,
    lists:foldl(F, [], RPCs).

next_signal(RPC, EncoderLib) ->
    F =
        "(" ++ untyped_fields_to_str(field_info({EncoderLib, RPC})) ++
            ")",
    "signal server_" ++ atom_to_list(RPC) ++ F.

next_signal_test() ->
    RPC = session_id,
    Encoder = overworld_pb,
    Expected = "signal server_session_id(id)",
    ?assertEqual(next_signal(RPC, Encoder), Expected).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup Packet Prefixes                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_prefixes() ->
    Apps = ow_protocol:apps(),
    F =
        fun({Prefix, {AppName, {_Module, _Decoder}}}, AccIn) ->
            AppString = atom_to_list(AppName),
            %PrefixPacked = integer_to_list(Prefix),
            [PrefixPacked] = erl_bin_to_godot(Prefix),
            Comment = "0x" ++ integer_to_list(Prefix, 16),
            Op =
                ?TAB ++ string:to_upper(AppString) ++ " = " ++ PrefixPacked ++
                    ", # " ++ Comment ++ "\n",
            Op ++ AccIn
        end,
    [chomp(lists:flatten(lists:foldl(F, [], Apps)))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Setup Packet Router                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_router() ->
    Apps = ow_protocol:apps(),
    F =
        fun({_Prefix, {AppName, {_Module, _Decoder}}}, AccIn) ->
            AppString = atom_to_list(AppName),
            Op =
                ?TAB(2) ++ "Prefix." ++ string:to_upper(AppString) ++ ":\n" ++
                    ?TAB(3) ++ "_server_" ++ AppString ++ "(payload)\n",
            [Op, AccIn]
        end,
    [chomp(lists:flatten(lists:foldl(F, [], Apps)))].

%generate_router(Operation, St0) ->
%    generate_router(Operation, [], St0).
%
%generate_router([], Routes, St0) ->
%    St0 ++ Routes;
%generate_router([OpInfo | Rest], Routes, St0) ->
%    OpName = opcode_name_string(OpInfo),
%    Op =
%        "OpCode." ++ string:to_upper(OpName) ++ ":\n" ++
%            ?TAB(3) ++ "_server_" ++ OpName ++
%            "(payload)",
%    generate_router(Rest, Routes, [Op | St0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate functions for unmarshalling server data                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example from GDScript:
% -----------------------
% func _server_overworld(packet):
%	print("Packet is: ", packet)
%	var m = Overworld_new_pb.overworld.new()
%	m.from_bytes(packet)
%	print(m)
%	var d = {}
%	d = unpack_overworld(m)
%	print(d)

generate_unmarshall() ->
    %    % Get RPCs that need unpacked
    %    Type = client,
    %    RPCs = ow_protocol:rpcs(Type),
    %    F = fun(RPC, Acc) ->
    %            #{encoder := Encoder} = ow_protocol:rpc(RPC, Type),
    %            [write_function(RPC, RPC, Encoder) | Acc]
    %    end,
    %    RPCUnpack = lists:foldl(F, [], RPCs),
    % Get apps that need unpacked
    Apps = ow_protocol:app_names(),
    G = fun(App, Acc) ->
        % TODO: Add a test for the encoder/0 function in the app for
        %       non-standard protobuf names
        Encoder = list_to_existing_atom(atom_to_list(App) ++ "_pb"),
        [write_app_function(App, Encoder) | Acc]
    end,
    AppUnpack = lists:foldl(G, [], Apps),
    [lists:flatten(AppUnpack)].

write_app_function(App, Encoder) ->
    EncStr = string:titlecase(atom_to_list(Encoder)),
    ProtoMsgStr = atom_to_list(App),
    "func " ++ "_server_" ++ ProtoMsgStr ++ "(packet):\n" ++
        ?TAB ++ "if debug:\n" ++
        ?TAB(2) ++ "print('[DEBUG] Processing a " ++ ProtoMsgStr ++
        " packet')\n" ++
        ?TAB ++ "var m = " ++ EncStr ++ "." ++ ProtoMsgStr ++
        ".new()\n" ++
        ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
        ?TAB ++ "if result_code != " ++ EncStr ++
        ".PB_ERR.NO_ERRORS:\n" ++
        ?TAB(2) ++ "print('[CRITICAL] Error decoding new " ++
        ProtoMsgStr ++ " packet')\n" ++
        ?TAB(2) ++ "return\n" ++
        ?TAB ++ "unpack_" ++ ProtoMsgStr ++ "(m)\n".

%write_function(ProtoMsg, ClientCall, Encoder) ->
%    EncStr = string:titlecase(atom_to_list(Encoder)),
%    ClientCallStr = atom_to_list(ClientCall),
%    ProtoMsgStr = atom_to_list(ProtoMsg),
%    Op =
%        "func " ++ "_server_" ++ ClientCallStr ++ "(packet):\n" ++
%            ?TAB ++ "if debug:\n" ++
%            ?TAB(2) ++ "print('[DEBUG] Processing a " ++ ClientCallStr ++
%            " packet')\n" ++
%            ?TAB ++ "var m = " ++ EncStr ++ "." ++ ProtoMsgStr ++
%            ".new()\n" ++
%            ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
%            ?TAB ++ "if result_code != " ++ EncStr ++
%            ".PB_ERR.NO_ERRORS:\n" ++
%            ?TAB(2) ++ "print('[CRITICAL] Error decoding new " ++
%            ClientCallStr ++ " packet')\n" ++
%            ?TAB(2) ++ "return\n",
%    Vars = unmarshall_var({Encoder, ProtoMsg}),
%    Signal =
%        ?TAB ++ "emit_signal('server_" ++ ProtoMsgStr ++ "'," ++
%            dict_fields_to_str(field_info({Encoder, ProtoMsg})) ++
%            "\)\n\n",
%    Op ++ Vars ++ Signal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate functions for marshalling submsgs                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_marshall_submsgs(Encoder) ->
    #{lib := EncoderLib} = Encoder,
    AllMessages = erlang:apply(EncoderLib, get_msg_names, []),
    generate_marshall_submsgs(AllMessages, Encoder, []).
generate_marshall_submsgs([], _Encoder, Acc) ->
    Acc;
generate_marshall_submsgs([vector2 | T], Encoder, Acc) ->
    Signature = "func pack_vector2(obj, ref):\n",
    Body =
        ?TAB ++ "ref.set_x(obj.x)\n" ++
            ?TAB ++ "ref.set_y(obj.y)\n",
    generate_marshall_submsgs(T, Encoder, Signature ++ Body ++ Acc);
generate_marshall_submsgs([MsgName | T], Encoder, Acc) ->
    #{lib := EncoderLib} = Encoder,
    Defn = erlang:apply(EncoderLib, fetch_msg_def, [MsgName]),
    NameStr = fix_delim(atom_to_list(MsgName)),
    Signature = "func pack_" ++ NameStr ++ "(obj, ref):\n",
    Body = marshall_submsg_body(Defn, []),
    generate_marshall_submsgs(T, Encoder, Signature ++ Body ++ Acc).

marshall_submsg_body([], Acc) ->
    Acc ++ "\n";
marshall_submsg_body([#{name := Name, type := {msg, SubMsg}} | T], Acc) ->
    NameStr = atom_to_list(Name),
    SubMsgStr = atom_to_list(SubMsg),
    Body =
        ?TAB ++ "var " ++ NameStr ++ " = ref.new_" ++ NameStr ++ "()\n" ++
            ?TAB ++ "pack_" ++ fix_delim(SubMsgStr) ++ "(obj['" ++ NameStr ++
            "'], " ++
            NameStr ++ ")\n",
    marshall_submsg_body(T, Body ++ Acc);
marshall_submsg_body([#{name := Name} | T], Acc) ->
    NameStr = atom_to_list(Name),
    Body = ?TAB ++ "ref.set_" ++ NameStr ++ "(obj." ++ NameStr ++ ")\n",
    marshall_submsg_body(T, Body ++ Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate functions for marshalling msgs                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example GDScript for encoding
%func session_id_req():
%	var m = Overworld_pb.overworld.new()
%	var n = m.new_session_id_req()
%	n.set_version(1)
%	var payload = m.to_bytes()
%	_send_message(payload, Prefix.OVERWORLD, 'reliable', 0)
%	if debug:
%		print('[INFO] Sent a session_id_req packet')

generate_marshall() ->
    Type = server,
    RPCs = ow_protocol:rpcs(Type),
    F = fun(RPC, Acc) ->
        #{encoder := Encoder, qos := QOS, channel := Channel} =
            ow_protocol:rpc(RPC, Type),
        #{app := App, lib := EncoderLib} = Encoder,
        FunStr = atom_to_list(RPC),
        Fields = field_info({EncoderLib, RPC}),
        FieldStr = fields_to_str(Fields),
        EncoderBare = erlang:atom_to_list(App),
        EncoderPrefix = string:to_upper(EncoderBare),
        EncoderTitle = string:titlecase(atom_to_list(EncoderLib)),
        Func =
            "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
                ?TAB ++ "var m = " ++ EncoderTitle ++ "." ++
                EncoderBare ++ ".new()\n" ++
                ?TAB ++ "var n = m.new_" ++ FunStr ++ "()\n" ++
                set_new_parameters(RPC, Encoder) ++
                ?TAB ++ "var payload = m.to_bytes()\n" ++
                ?TAB ++ "_send_message(payload, Prefix." ++ EncoderPrefix ++
                ", '" ++ atom_to_list(QOS) ++ "', " ++
                integer_to_list(Channel) ++ ")\n" ++
                ?TAB ++ "if debug:\n" ++
                ?TAB(2) ++ "print('[INFO] Send a " ++ FunStr ++
                " packet')\n\n",
        [Func | Acc]
    end,
    [lists:flatten(lists:foldl(F, [], RPCs))].

%generate_marshall() ->
%    Type = server,
%    RPCs = ow_protocol:rpcs(Type),
%    F = fun(RPC, Acc) ->
%        #{encoder := Encoder, qos := QOS, channel := Channel} =
%            ow_protocol:rpc(RPC, Type),
%        %FunStr = opcode_name_string(RPC),
%        FunStr = atom_to_list(RPC),
%        Fields = field_info({Encoder, RPC}),
%        FieldStr = fields_to_str(Fields),
%        EncStr = string:titlecase(atom_to_list(Encoder)),
%        Func =
%            "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
%                ?TAB ++ "var m = " ++ EncStr ++ "." ++
%                atom_to_list(RPC) ++
%                ".new()\n" ++
%                set_new_parameters(RPC, Encoder) ++
%                %set_parameters(Fields, Encoder) ++
%                ?TAB ++ "var payload = m.to_bytes()\n" ++
%                ?TAB ++ "_send_message(payload, Prefix." ++
%                string:to_upper(FunStr) ++ ", '" ++ atom_to_list(QOS) ++
%                "', " ++ integer_to_list(Channel) ++ ")\n" ++
%                ?TAB ++ "if debug:\n" ++
%                ?TAB(2) ++ "print('[INFO] Sent a " ++
%                FunStr ++
%                " packet')\n\n",
%        [Func | Acc]
%    end,
%    [lists:flatten(lists:foldl(F, [], RPCs))].

set_new_parameters(ClientMsg, Encoder) ->
    #{lib := EncoderLib} = Encoder,
    Defn = erlang:apply(EncoderLib, fetch_msg_def, [ClientMsg]),
    parameter_body(Defn, []).
parameter_body([], Acc) ->
    Acc;
parameter_body([#{name := Name, type := {msg, SubMsg}} | T], Acc) ->
    NameStr = atom_to_list(Name),
    B =
        ?TAB ++ "pack_" ++ atom_to_list(SubMsg) ++ "(" ++ NameStr ++
            ", n.new_" ++
            NameStr ++ "())\n",
    parameter_body(T, B ++ Acc);
parameter_body([#{name := Name, occurrence := Occurrence} | T], Acc) ->
    B =
        case Occurrence of
            optional ->
                % If it's null, don't set it because the encoder can't handle nulls
                % TODO: Nulls throw an error per
                %       https://github.com/godotengine/godot/issues/56217
                ?TAB ++ atom_to_list(Name) ++ "=" ++ atom_to_list(Name) ++
                    "\n" ++
                    ?TAB ++ "if " ++ atom_to_list(Name) ++ ":\n" ++ ?TAB ++
                    ?TAB ++
                    "n.set_" ++ atom_to_list(Name) ++ "(" ++
                    atom_to_list(Name) ++
                    ")\n";
            repeated ->
                % If it's repeated type, we need to add instead of set
                ?TAB ++ "for item in " ++ atom_to_list(Name) ++ ":\n" ++
                    ?TAB ++ ?TAB ++ "m.add_" ++ atom_to_list(Name) ++
                    "(item)\n";
            _ ->
                ?TAB ++ "n.set_" ++ atom_to_list(Name) ++ "(" ++
                    atom_to_list(Name) ++ ")\n"
        end,
    parameter_body(T, B ++ Acc).

%
% This function has been heavily retrofitted to allow for optional arguments,
% but you'll need to be careful in the order in which they are specified in the
% protobuf message because we don't do any ordering here, and optional
% arguments in GDScript aren't quite like named arguments in Python, for
% example.
%
% TODO: Investigate sorting arguments, such that required ones always come first.
fields_to_str(List) ->
    fields_to_str(List, "").
fields_to_str([], Acc) ->
    Acc;
fields_to_str([{N, T, O} | Tail], "") ->
    Name = atom_to_list(N),
    Acc1 =
        case O of
            required ->
                case T of
                    {enum, _} ->
                        Name;
                    string ->
                        Name ++ ": " ++ "String";
                    int64 ->
                        Name ++ ": " ++ "int";
                    uint64 ->
                        Name ++ ": " ++ "int";
                    sint64 ->
                        Name ++ ": " ++ "int";
                    sint32 ->
                        Name ++ ": " ++ "int";
                    uint32 ->
                        Name ++ ": " ++ "int";
                    Type ->
                        Name ++ ": " ++ maybe_submsg(Type)
                end;
            repeated ->
                Name ++ ": Array";
            optional ->
                % If the parameter is optional, set the parameter to =Null and use no typing
                case T of
                    string ->
                        Name ++ " = ''";
                    _ ->
                        Name ++ " = null"
                end
        end,
    fields_to_str(Tail, Acc1);
fields_to_str([{N, T, O} | Tail], Acc) ->
    Name = atom_to_list(N),
    Acc1 =
        case O of
            required ->
                case T of
                    {enum, _} ->
                        Name ++ ", " ++ Acc;
                    string ->
                        Name ++ ": " ++ "String" ++ ", " ++ Acc;
                    int64 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    uint64 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    sint64 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    uint32 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    sint32 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    Type ->
                        Name ++ ": " ++ maybe_submsg(Type) ++ ", " ++ Acc
                end;
            repeated ->
                Name ++ ": " ++ "Array, " ++ Acc;
            optional ->
                % If the parameter is optional, set the parameter to =Null and use no typing
                Name ++ " = null, " ++ Acc
        end,
    fields_to_str(Tail, Acc1).

dict_fields_to_str(List) ->
    dict_fields_to_str(List, []).
dict_fields_to_str([], Acc) ->
    % join the results into a string
    string:join(Acc, ", ");
dict_fields_to_str([{N, _T, _O} | Tail], Acc) ->
    D = "d['" ++ atom_to_list(N) ++ "']",
    dict_fields_to_str(Tail, [D | Acc]).

untyped_fields_to_str(List) ->
    untyped_fields_to_str(lists:reverse(List), "").
untyped_fields_to_str([], Acc) ->
    Acc;
untyped_fields_to_str([{N, _T, _O} | Tail], "") ->
    Acc1 = atom_to_list(N),
    untyped_fields_to_str(Tail, Acc1);
untyped_fields_to_str([{N, _T, _O} | Tail], Acc) ->
    Name = atom_to_list(N),
    Acc1 = Acc ++ "," ++ Name,
    untyped_fields_to_str(Tail, Acc1).

% case where a function has no arguments and therefore has no protobuf message.
% i.e., ping
field_info({undefined, _ProtoMsg}) ->
    field_info([], []);
field_info({EncoderLib, ProtoMsg}) ->
    Defs = erlang:apply(EncoderLib, fetch_msg_def, [ProtoMsg]),
    field_info(Defs, []).

field_info([], Acc) ->
    Acc;
field_info([H | T], Acc) ->
    Name = maps:get(name, H),
    Type = maps:get(type, H),
    Occurrence = maps:get(occurrence, H),
    Acc1 = [{Name, Type, Occurrence} | Acc],
    field_info(T, Acc1).

%unmarshall_var({ProtoLib, ProtoMsg}) ->
%    unmarshall_var(
%        field_info({ProtoLib, ProtoMsg}), ProtoMsg, ProtoLib, []
%    ).
%unmarshall_var([], _ProtoMsg, _ProtoLib, Acc) ->
%    Acc;
%unmarshall_var([{_F, _T, _O} | _Rest], ProtoMsg, ProtoLib, Acc) ->
%    V =
%        ?TAB ++ "var d = {}\n" ++ ?TAB ++ "d = unpack_" ++
%            atom_to_list(ProtoMsg) ++ "(m)\n",
%    unmarshall_var([], ProtoMsg, ProtoLib, Acc ++ V).

%opcode_name_string(OpInfo) ->
%    OpCode = ow_rpc:opcode(OpInfo),
%    case ow_rpc:c2s_handler(OpInfo) of
%        {_M, F, _A} ->
%            atom_to_list(F);
%        undefined ->
%            % Try the next best guess
%            case ow_rpc:s2c_call(OpInfo) of
%                undefined -> "undefined_" ++ integer_to_list(OpCode);
%                Call -> atom_to_list(Call)
%            end
%    end.

% Make a best guess at a fall through for the encoder. I'm not sure I like this so it's not part of the main RPC module.
%correct_encoder(undefined, _) ->
%    undefined;
%correct_encoder(_, undefined) ->
%    undefined;
%correct_encoder(Encoder, Message) ->
%    #{ lib := EncoderLib } = Encoder,
%    case erlang:apply(EncoderLib, find_msg_def, [Message]) of
%        error ->
%            logger:debug(
%                "Couldn't find message ~p for encoder ~p, assuming encoder is ~p!~n",
%                [Message, EncoderLib, ?DEFAULT_ENCODER]
%            ),
%            ?DEFAULT_ENCODER;
%        _ ->
%            EncoderLib
%    end.

maybe_submsg({msg, _Type}) ->
    % Do nothing to submessages. Provide a helper function somewhere.
    atom_to_list('Dictionary');
maybe_submsg(Type) ->
    atom_to_list(Type).

fix_delim(Message) ->
    lists:flatten(string:replace(Message, ".", "_")).

erl_bin_to_godot(Bin) ->
    B = binary_to_list(binary:encode_unsigned(Bin, big)),
    if
        length(B) < 2 ->
            io_lib:format("~p", [[0 | B]]);
        true ->
            io_lib:format("~p", [B])
    end.

chomp(String) ->
    lists:droplast(String).
