-module(ow_binding).

% This module generates GDScript code to handle game-specific signaling.  The
% module in its 3rd major overhaul and shows no signs of being done.  I have no
% doubt there will be even more to do with Godot 4.0

-export([
    write/0,
    pb_to_godot_type/1,
    print/1,
    print/0
]).

-define(TAB, [9]).
-define(TAB(N),
    lists:foldl(fun(_N, Acc0) -> [9] ++ Acc0 end, [], lists:seq(0, N - 1))
).
-define(DEFAULT_ENCODER, overworld_pb).
-define(DEFAULT_TEMPLATE_3, "templates/libow3.mustache").
-define(DEFAULT_TEMPLATE_4, "templates/libow4.mustache").

write() ->
    file:write_file(
        "priv/static/libow.gd", ow_binding:print()
    ).

print() ->
    print(3).
print(Version) ->
    Ops = [
        ow_protocol:op_info(X)
     || X <- ow_protocol:registered_ops()
    ],
    Encoders = get_encoders(Ops),
    Preloads = load_scripts(Ops),
    Enums = generate_enums(Ops),
    Signals = generate_signals(Ops, []),
    Opcodes = generate_opcodes(Ops, []),
    Router = generate_router(Ops, []),
    Submsgs = [generate_submsgs(E) || E <- Encoders],
    Unmarshall = generate_unmarshall(Ops, []),
    MarshallSubmsgs = [generate_marshall_submsgs(E) || E <- Encoders],
    Marshall = generate_marshall(Ops, []),
    Map = #{
        "preloads" => Preloads,
        "constants" => Enums,
        "signals" => Signals,
        "opcodes" => Opcodes,
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

get_encoders(Ops) ->
    get_encoders(Ops, []).
get_encoders([], Acc) ->
    Acc;
get_encoders([H | T], Acc) ->
    E =
        case maps:get(encoder, H, undefined) of
            undefined -> overworld_pb;
            Encoder -> Encoder
        end,
    Acc1 =
        case lists:member(E, Acc) of
            false ->
                [E | Acc];
            true ->
                Acc
        end,
    get_encoders(T, Acc1).

filter_for_pure_msgs(Encoder) ->
    MsgList = erlang:apply(Encoder, get_msg_defs, []),
    filter_for_pure_msgs(MsgList, []).
filter_for_pure_msgs([], Acc) ->
    Acc;
filter_for_pure_msgs([{{enum, _}, _} | Rest], Acc) ->
    % Skip enums (for now?)
    filter_for_pure_msgs(Rest, Acc);
filter_for_pure_msgs([{{msg, Name}, MapList} | Rest], Acc) ->
    Predicate = fun(Map) ->
        T = maps:get(type, Map),
        case T of
            {msg, _} -> false;
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
    Defn = erlang:apply(Encoder, fetch_msg_def, [MessageName]),
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
    AllMessages = erlang:apply(Encoder, get_msg_names, []),
    Impures = AllMessages -- Pures,
    generate_impure_submsgs(Encoder, Impures, []).
generate_impure_submsgs(_Encoder, [], Acc) ->
    Acc;
generate_impure_submsgs(Encoder, [H | T], Acc) ->
    Defn = erlang:apply(Encoder, fetch_msg_def, [H]),
    Signature =
        "func unpack_" ++ fix_delim(atom_to_list(H)) ++ "(object):\n",
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
    generate_impure_submsgs(Encoder, T, Signature ++ Body ++ Acc).

generate_enums(Ops) ->
    generate_enums(Ops, [], []).
generate_enums([], _Seen, Acc) ->
    Acc;
generate_enums([H | T], Seen, Acc) ->
    ProtoLib = get_encoder(H),
    case lists:member(ProtoLib, Seen) of
        true ->
            % Already processed this protobuf file, skip
            generate_enums(T, Seen, Acc);
        false ->
            Enums = erlang:apply(ProtoLib, get_enum_names, []),
            % Process all enums
            Comment = "# via " ++ atom_to_list(ProtoLib),
            Acc1 = Acc ++ [Comment] ++ stringify_enums(ProtoLib, Enums),
            generate_enums(T, [ProtoLib | Seen], Acc1)
    end.

stringify_enums(ProtoLib, Enums) ->
    stringify_enums(ProtoLib, Enums, []).
stringify_enums(_ProtoLib, [], Acc) ->
    [Acc];
stringify_enums(ProtoLib, [H | T], Acc) ->
    % E has the structure [{atom(), non_negative_integer()}, ...]
    EnumName = string:replace(atom_to_list(H), ".", "_"),
    EncStr = string:titlecase(atom_to_list(ProtoLib)),
    Prefix = "enum " ++ EnumName ++ " {\n",
    E = erlang:apply(ProtoLib, fetch_enum_def, [H]),
    Estr = [
        ?TAB ++ atom_to_list(Name) ++ " = " ++ EncStr ++ "." ++
            atom_to_list(H) ++ "." ++ atom_to_list(Name) ++ ",\n"
     || {Name, _Value} <- E
    ],
    Acc1 = Acc ++ Prefix ++ lists:flatten(Estr) ++ "}\n",
    stringify_enums(ProtoLib, T, Acc1).

get_encoder(Op) ->
    case ow_rpc:encoder(Op) of
        undefined ->
            ?DEFAULT_ENCODER;
        Encoder ->
            Encoder
    end.

load_scripts(Ops) ->
    load_scripts(Ops, [], []).
load_scripts([], _Seen, Acc) ->
    lists:reverse(Acc);
load_scripts([H | T], Seen, Acc) ->
    case ow_rpc:encoder(H) of
        undefined ->
            % do we need to do anthing for the default case?  e.g., ow_pb
            % TODO: If so, maybe use the get_encoder/1 fun
            load_scripts(T, Seen, Acc);
        Encoder ->
            case lists:member(Encoder, Seen) of
                false ->
                    Const =
                        "const " ++ string:titlecase(atom_to_list(Encoder)) ++
                            " = preload('",
                    Script = atom_to_list(Encoder) ++ ".gd')",
                    Seen1 = [Encoder | Seen],
                    load_scripts(T, Seen1, [Const ++ Script | Acc]);
                true ->
                    load_scripts(T, Seen, Acc)
            end
    end.

generate_signals(OpInfo, St0) ->
    generate_signals(OpInfo, [], St0).

generate_signals([], _Seen, St0) ->
    lists:reverse(St0);
generate_signals([OpInfo | Rest], SignalsSeen0, St0) ->
    % Server ---> Client
    Encoder = ow_rpc:encoder(OpInfo),
    MsgFromServer = ow_rpc:s2c_call(OpInfo),
    case lists:member(MsgFromServer, SignalsSeen0) of
        true ->
            % Already seen this signal
            generate_signals(Rest, SignalsSeen0, St0);
        false ->
            St1 = next_signal(MsgFromServer, Encoder, St0),
            SignalsSeen1 = [MsgFromServer | SignalsSeen0],
            generate_signals(Rest, SignalsSeen1, St1)
    end.

next_signal(MsgFromServer, Encoder, St0) when
    Encoder == undefined; MsgFromServer == undefined
->
    % If there's no msg to be decoded, there's no relevant signal to send.
    % TODO: verify this
    %Signal = "signal " ++ atom_to_list(MsgFromServer),
    St0;
next_signal(MsgFromServer, Encoder, St0) ->
    F =
        "(" ++ untyped_fields_to_str(field_info({Encoder, MsgFromServer})) ++
            ")",
    Signal = "signal server_" ++ atom_to_list(MsgFromServer) ++ F,
    [Signal | St0].

generate_opcodes(Ops, St0) ->
    next_opcode(Ops, ok, St0).

next_opcode([], ok, St0) ->
    lists:reverse(St0);
next_opcode([OpInfo | Rest], ok, St0) ->
    OpCode = ow_rpc:opcode(OpInfo),
    OpName = opcode_name_string(OpInfo),
    %Op =
    %    string:to_upper(OpName) ++ " = " ++
    %        "bytepack(" ++ OpString ++ "),",
    OpPacked = erl_bin_to_godot(OpCode),
    Comment = "0x" ++ integer_to_list(OpCode, 16),
    Op = string:to_upper(OpName) ++ " = " ++ OpPacked ++ ", # " ++ Comment,
    next_opcode(Rest, ok, [Op | St0]).

generate_router(Operation, St0) ->
    generate_router(Operation, [], St0).

generate_router([], Routes, St0) ->
    St0 ++ Routes;
generate_router([OpInfo | Rest], Routes, St0) ->
    OpName = opcode_name_string(OpInfo),
    Op =
        "OpCode." ++ string:to_upper(OpName) ++ ":\n" ++
            ?TAB(3) ++ "_server_" ++ OpName ++
            "(payload)",
    generate_router(Rest, Routes, [Op | St0]).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall([OpInfo | Rest], St0) ->
    ServerMsg = ow_rpc:s2c_call(OpInfo),
    Encoder = correct_encoder(ow_rpc:encoder(OpInfo), ServerMsg),
    %FunStr = opcode_name_string(OpInfo),
    OpFun = rpc_name(OpInfo),
    St1 = write_function(ServerMsg, OpFun, Encoder, St0),
    generate_unmarshall(Rest, St1).

write_function(undefined, ClientCall, _Encoder, St0) ->
    % There's no sensible message to unpack or handler to write.
    % We still need to make a handler that can understand the opcode because
    % the message router expects one.
    ClientCallStr = atom_to_list(ClientCall),
    Op =
        "func " ++ "_server_" ++ ClientCallStr ++ "(_packet):\n" ++
            ?TAB ++ "print('[WARN] Received a " ++
            ClientCallStr ++ " packet')\n" ++
            ?TAB ++ "return\n",
    [Op | St0];
write_function(ProtoMsg, ClientCall, Encoder, St0) ->
    EncStr = string:titlecase(atom_to_list(Encoder)),
    ClientCallStr = atom_to_list(ClientCall),
    ProtoMsgStr = atom_to_list(ProtoMsg),
    Op =
        "func " ++ "_server_" ++ ClientCallStr ++ "(packet):\n" ++
            ?TAB ++ "if debug:\n" ++
            ?TAB(2) ++ "print('[DEBUG] Processing a " ++ ClientCallStr ++
            " packet')\n" ++
            ?TAB ++ "var m = " ++ EncStr ++ "." ++ ProtoMsgStr ++
            ".new()\n" ++
            ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
            ?TAB ++ "if result_code != " ++ EncStr ++
            ".PB_ERR.NO_ERRORS:\n" ++
            ?TAB(2) ++ "print('[CRITICAL] Error decoding new " ++
            ClientCallStr ++ " packet')\n" ++
            ?TAB(2) ++ "return\n",
    Vars = unmarshall_var({Encoder, ProtoMsg}),
    Signal =
        ?TAB ++ "emit_signal('server_" ++ ProtoMsgStr ++ "'," ++
            dict_fields_to_str(field_info({Encoder, ProtoMsg})) ++
            "\)\n\n",
    [Op ++ Vars ++ Signal | St0].

generate_marshall_submsgs(Encoder) ->
    AllMessages = erlang:apply(Encoder, get_msg_names, []),
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
    Defn = erlang:apply(Encoder, fetch_msg_def, [MsgName]),
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

generate_marshall([], St0) ->
    lists:reverse(St0);
generate_marshall(
    [OpInfo | Rest], St0
) ->
    case ow_rpc:c2s_handler(OpInfo) of
        undefined ->
            % No message to pack
            generate_marshall(Rest, St0);
        {_, ClientMsg, _} ->
            FunStr = opcode_name_string(OpInfo),
            Encoder = ow_rpc:encoder(OpInfo),
            QOS =
                case ow_rpc:qos(OpInfo) of
                    undefined -> "reliable";
                    Type -> atom_to_list(Type)
                end,
            Channel =
                case ow_rpc:channel(OpInfo) of
                    undefined -> "0";
                    Number -> integer_to_list(Number)
                end,
            Op =
                case Encoder of
                    undefined ->
                        % define an empty message for ping
                        "func " ++ FunStr ++ "():\n" ++
                            ?TAB ++ "_send_message([], OpCode." ++
                            string:to_upper(FunStr) ++ ", '" ++ QOS ++
                            "', " ++ Channel ++ ")\n" ++
                            ?TAB ++ "if debug:\n" ++
                            ?TAB(2) ++ "print('[INFO] Sent a " ++
                            FunStr ++
                            " packet')\n\n";
                    _ ->
                        % Allow the client message name to be overriden, optionally
                        ProtoMsg =
                            case ow_rpc:c2s_proto(OpInfo) of
                                % If there's no override specified, just
                                % go with the name of the handler as the
                                % name of the message
                                undefined -> ClientMsg;
                                Proto -> Proto
                            end,
                        Fields = field_info({Encoder, ProtoMsg}),
                        FieldStr = fields_to_str(Fields),
                        EncStr = string:titlecase(atom_to_list(Encoder)),
                        "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
                            ?TAB ++ "var m = " ++ EncStr ++ "." ++
                            atom_to_list(ProtoMsg) ++
                            ".new()\n" ++
                            set_new_parameters(ProtoMsg, Encoder) ++
                            %set_parameters(Fields, Encoder) ++
                            ?TAB ++ "var payload = m.to_bytes()\n" ++
                            ?TAB ++ "_send_message(payload, OpCode." ++
                            string:to_upper(FunStr) ++ ", '" ++ QOS ++
                            "', " ++ Channel ++ ")\n" ++
                            ?TAB ++ "if debug:\n" ++
                            ?TAB(2) ++ "print('[INFO] Sent a " ++
                            FunStr ++
                            " packet')\n\n"
                end,
            generate_marshall(Rest, [Op | St0])
    end.

set_new_parameters(ClientMsg, Encoder) ->
    Defn = erlang:apply(Encoder, fetch_msg_def, [ClientMsg]),
    parameter_body(Defn, []).
parameter_body([], Acc) ->
    Acc;
parameter_body([#{name := Name, type := {msg, SubMsg}} | T], Acc) ->
    NameStr = atom_to_list(Name),
    B =
        ?TAB ++ "pack_" ++ atom_to_list(SubMsg) ++ "(" ++ NameStr ++
            ", m.new_" ++
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
                    "m.set_" ++ atom_to_list(Name) ++ "(" ++
                    atom_to_list(Name) ++
                    ")\n";
            repeated ->
                % If it's repeated type, we need to add instead of set
                ?TAB ++ "for item in " ++ atom_to_list(Name) ++ ":\n" ++
                    ?TAB ++ ?TAB ++ "m.add_" ++ atom_to_list(Name) ++
                    "(item)\n";
            _ ->
                ?TAB ++ "m.set_" ++ atom_to_list(Name) ++ "(" ++
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
    untyped_fields_to_str(List, "").
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
field_info({ProtoLib, ProtoMsg}) ->
    E = correct_encoder(ProtoLib, ProtoMsg),
    Defs = erlang:apply(E, fetch_msg_def, [ProtoMsg]),
    field_info(Defs, []).

field_info([], Acc) ->
    Acc;
field_info([H | T], Acc) ->
    Name = maps:get(name, H),
    Type = maps:get(type, H),
    Occurrence = maps:get(occurrence, H),
    Acc1 = [{Name, Type, Occurrence} | Acc],
    field_info(T, Acc1).

unmarshall_var({ProtoLib, ProtoMsg}) ->
    unmarshall_var(
        field_info({ProtoLib, ProtoMsg}), ProtoMsg, ProtoLib, []
    ).
unmarshall_var([], _ProtoMsg, _ProtoLib, Acc) ->
    Acc;
unmarshall_var([{_F, _T, _O} | _Rest], ProtoMsg, ProtoLib, Acc) ->
    V =
        ?TAB ++ "var d = {}\n" ++ ?TAB ++ "d = unpack_" ++
            atom_to_list(ProtoMsg) ++ "(m)\n",
    unmarshall_var([], ProtoMsg, ProtoLib, Acc ++ V).

opcode_name_string(OpInfo) ->
    OpCode = ow_rpc:opcode(OpInfo),
    case ow_rpc:c2s_handler(OpInfo) of
        {_M, F, _A} ->
            atom_to_list(F);
        undefined ->
            % Try the next best guess
            case ow_rpc:s2c_call(OpInfo) of
                undefined -> "undefined_" ++ integer_to_list(OpCode);
                Call -> atom_to_list(Call)
            end
    end.

rpc_name(OpInfo) ->
    OpCode = ow_rpc:opcode(OpInfo),
    case ow_rpc:c2s_handler(OpInfo) of
        {_M, F, _A} ->
            F;
        undefined ->
            % Try the next best guess
            case ow_rpc:s2c_call(OpInfo) of
                undefined -> "undefined_" ++ integer_to_list(OpCode);
                Call -> Call
            end
    end.

% Make a best guess at a fall through for the encoder. I'm not sure I like this so it's not part of the main RPC module.
correct_encoder(undefined, _) ->
    undefined;
correct_encoder(_, undefined) ->
    undefined;
correct_encoder(Encoder, Message) ->
    case erlang:apply(Encoder, find_msg_def, [Message]) of
        error ->
            logger:debug(
                "Couldn't find message ~p for encoder ~p, assuming encoder is ~p!~n",
                [Message, Encoder, ?DEFAULT_ENCODER]
            ),
            ?DEFAULT_ENCODER;
        _ ->
            Encoder
    end.

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
