-module(gremlin_binding).

% this module is heavily iterated upon and is in its 2nd major version.
% it'll need a third major version to be really solid :)

-export([
    write/0,
    pb_to_godot_type/1,
    print/0
]).

-define(TAB, [9]).
-define(TAB(N),
    lists:foldl(fun(_N, Acc0) -> [9] ++ Acc0 end, [], lists:seq(0, N - 1))
).
-define(DEFAULT_ENCODER, gremlin_pb).

write() ->
    file:write_file(
        "apps/gremlin_core/priv/static/libgremlin.gd", gremlin_binding:print()
    ).

print() ->
    Ops = [
        gremlin_protocol:op_info(X)
     || X <- gremlin_protocol:registered_ops()
    ],
    Encoders = get_encoders(Ops),
    Preloads = load_scripts(Ops),
    Enums = generate_enums(Ops),
    Signals = generate_signals(Ops, []),
    Opcodes = generate_opcodes(Ops, []),
    Router = generate_router(Ops, []),
    Submsgs = [ generate_submsgs(E) || E <- Encoders ],
    Unmarshall = generate_unmarshall(Ops, []),
    Marshall = generate_marshall(Ops, []),
    Map = #{
        "preloads" => Preloads,
        "constants" => Enums,
        "signals" => Signals,
        "opcodes" => Opcodes,
        "router" => Router,
        "submsgs" => Submsgs,
        "unmarshall" => Unmarshall,
        "marshall" => Marshall
    },
    T = bbmustache:parse_file(
        "apps/gremlin_core/templates/libgremlin.mustache"
    ),
    bbmustache:compile(T, Map).


pb_to_godot_type(Type) ->
    case Type of
        double -> real;
        float -> real;
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
        bytes -> 'PoolByteArray';
        _Other -> void % Do the best you can. 
    end.


get_encoders(Ops) ->
    get_encoders(Ops, []).
get_encoders([], Acc) ->
    Acc;
get_encoders([H|T], Acc) -> 
    E = 
        case maps:get(encoder, H, undefined) of
            undefined -> gremlin_pb;
            Encoder -> Encoder
        end,
    Acc1 = 
        case lists:member(E, Acc) of
            false -> 
                [ E | Acc ];
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
filter_for_pure_msgs([{{msg,Name}, MapList} | Rest], Acc) ->
    Predicate = fun(Map) -> 
                        T = maps:get(type, Map),
                        case T of
                            {msg, _} -> false;
                            _ -> true
                        end
                end,
    FilteredList = lists:filter(Predicate, MapList),
    % If the lists match, then the message was impure and we should skip it.
    Acc1 = case FilteredList of
               MapList -> [ Name | Acc ];
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
    % not understood by Erlang
    Signature = "func unpack_vector2(object)\n",
    Body = ?TAB ++ "var vec = Vector2(object.get_x(), object.get_y())\n",
    Return = ?TAB ++ "return vec\n",
    generate_pure_submsgs(Encoder, Rest, [ Signature ++ Body ++ Return | Acc ]);
generate_pure_submsgs(Encoder, [MessageName | Rest], Acc) ->
    Defn = erlang:apply(Encoder, fetch_msg_def, [MessageName]),
    Signature= "func unpack_" ++ atom_to_list(MessageName) ++ "(object):\n",
    Body = generate_submsg_body(Defn, []),
    Dict = generate_submsg_dict(Defn),
    generate_pure_submsgs(Encoder, Rest, Signature ++ Body ++ Dict ++ Acc).

generate_submsg_body([], Acc) ->
    Acc;
generate_submsg_body([Defn = #{type := {msg, Submsg}} |T], Acc) ->
    % Impure submsg does not have well known types and we need to call one of
    % the lower-level functions to deal with it
    Name = atom_to_list(maps:get(name, Defn)),
    Type = atom_to_list(Submsg),
    Body = ?TAB ++ "var " ++ Name ++ " = unpack_" ++ Type ++ "(object.get_" ++ Name ++ "())\n",
    generate_submsg_body(T, Body ++ Acc);
generate_submsg_body([H|T], Acc) ->
    io:format("message is ~p~n", [H]),
    % All members of a pure submsg are of well-known types
    Name = atom_to_list(maps:get(name, H)),
    % Godobuf should automagically generate arrays as appropriate for
    % well-knowns, so no need to special case these.
    Body = ?TAB ++ "var " ++ Name ++ " = object.get_" ++ Name ++ "()\n",
    generate_submsg_body(T, Body ++ Acc).

generate_submsg_dict(Definitions) ->
    Pre = ?TAB ++ "var dict = {",
    generate_submsg_dict(Definitions, Pre).
generate_submsg_dict([], Acc) ->
    Acc ++ "}\n" ++
    ?TAB ++ "return dict\n";
generate_submsg_dict([H|T], Acc) -> 
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
generate_impure_submsgs(Encoder, [H|T],Acc) ->
    Defn = erlang:apply(Encoder, fetch_msg_def, [H]),
    Signature= "func unpack_" ++ atom_to_list(H) ++ "(object):\n",
    io:format("Function sig is: ~p~n", [Signature]),
    Body = generate_submsg_body(Defn, []),
    Dict = generate_submsg_dict(Defn),
    generate_impure_submsgs(Encoder, T, Signature ++ Body ++ Dict ++ Acc).

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
    case gremlin_rpc:encoder(Op) of
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
    case gremlin_rpc:encoder(H) of
        undefined ->
            % do we need to do anthing for the default case?  e.g., gremlin_pb
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
    Encoder = gremlin_rpc:encoder(OpInfo),
    MsgFromServer = gremlin_rpc:s2c_call(OpInfo),
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
    Signal = "signal " ++ atom_to_list(MsgFromServer) ++ F,
    [Signal | St0].

generate_opcodes(Ops, St0) ->
    next_opcode(Ops, ok, St0).

next_opcode([], ok, St0) ->
    lists:reverse(St0);
next_opcode([OpInfo | Rest], ok, St0) ->
    OpCode = gremlin_rpc:opcode(OpInfo),
    OpName = opcode_name_string(OpInfo),
    OpString = io_lib:format("~p", [OpCode]),
    Op =
        string:to_upper(OpName) ++ " = " ++
            "bytepack(" ++ OpString ++ "),",
    next_opcode(Rest, ok, [Op | St0]).

generate_router(Operation, St0) ->
    generate_router(Operation, [], St0).

generate_router([], Routes, St0) ->
    St0 ++ Routes;
generate_router([OpInfo | Rest], Routes, St0) ->
    OpName = opcode_name_string(OpInfo),
    Op =
        "OpCode." ++ string:to_upper(OpName) ++ ":\n" ++
            ?TAB(3) ++ "server_" ++ OpName ++
            "(payload)",
    generate_router(Rest, Routes, [Op | St0]).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall([OpInfo | Rest], St0) ->
    ServerMsg = gremlin_rpc:s2c_call(OpInfo),
    Encoder = correct_encoder(gremlin_rpc:encoder(OpInfo), ServerMsg),
    %FunStr = opcode_name_string(OpInfo),
    OpFun = rpc_name(OpInfo),
    St1 = write_function(ServerMsg, OpFun, Encoder, St0),
    generate_unmarshall(Rest, St1).

write_function(undefined, _Undefined, _Encoder, St0) ->
    % There's no sensible message to unpack or handler to write. Just pass the
    % current state forward.
    St0;
write_function(ProtoMsg, ClientCall, Encoder, St0) ->
    EncStr = string:titlecase(atom_to_list(Encoder)),
    ClientCallStr = atom_to_list(ClientCall),
    ProtoMsgStr = atom_to_list(ProtoMsg),
    Op =
        "func " ++ "server_" ++ ClientCallStr ++ "(packet):\n" ++
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
        ?TAB ++ "emit_signal('" ++ ProtoMsgStr ++ "'," ++
            dict_fields_to_str(field_info({Encoder, ProtoMsg})) ++
            "\)\n\n",
    [ Op ++ Vars ++ Signal | St0 ].




%write_function(undefined, undefined, _Encoder, Rest, St0) ->
%    % No message to unpack, no sensible name to decode. Assume this is a
%    % message only meant to be *sent* to the server
%    generate_unmarshall(Rest, St0);
%write_function(undefined, FunStr, _Encoder, Rest, St0) ->
%    % In this case, there's a named function but no servermsg. We can safely
%    % assume that there's simply no arguments for this fun.
%    Op =
%        "func " ++ "server_" ++ FunStr ++ "(_packet):\n" ++
%            ?TAB ++ "print('[WARN] Received a " ++
%            FunStr ++ " packet')\n",
%    generate_unmarshall(Rest, [Op | St0]);
%write_function(ServerMsg, FunStr, Encoder, Rest, St0) ->
%    EncStr = string:titlecase(atom_to_list(Encoder)),
%    Op =
%        "func " ++ "server_" ++ FunStr ++ "(packet):\n" ++
%            ?TAB ++ "if debug:\n" ++
%            ?TAB(2) ++ "print('[DEBUG] Processing a " ++ FunStr ++
%            " packet')\n" ++
%            ?TAB ++ "var m = " ++ EncStr ++ "." ++ atom_to_list(ServerMsg) ++
%            ".new()\n" ++
%            ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
%            ?TAB ++ "if result_code != " ++ EncStr ++
%            ".PB_ERR.NO_ERRORS:\n" ++
%            ?TAB(2) ++ "print('[CRITICAL] Error decoding new " ++
%            FunStr ++ " packet')\n" ++
%            ?TAB(2) ++ "return\n",
%    Vars = unmarshall_var({Encoder, ServerMsg}),
%    %Signal =
%    %    ?TAB ++ "emit_signal('" ++ atom_to_list(ServerMsg) ++ "'," ++
%    %        untyped_fields_to_str(field_info({Encoder, ServerMsg})) ++
%    %        "\)\n\n",
%    Signal =
%        ?TAB ++ "emit_signal('" ++ atom_to_list(ServerMsg) ++ "'," ++
%            dict_fields_to_str(field_info({Encoder, ServerMsg})) ++
%            "\)\n\n",
%    generate_unmarshall(Rest, [Op ++ Vars ++ Signal | St0]).

generate_marshall([], St0) ->
    lists:reverse(St0);
generate_marshall(
    [OpInfo | Rest], St0
) ->
    ClientMsg = gremlin_rpc:c2s_call(OpInfo),
    case ClientMsg of
        undefined ->
            % No message to pack
            generate_marshall(Rest, St0);
        _ ->
            FunStr = opcode_name_string(OpInfo),
            Encoder = gremlin_rpc:encoder(OpInfo),
            Op =
                case Encoder of
                    undefined ->
                        % define an empty message for ping
                        "func " ++ FunStr ++ "():\n" ++
                            ?TAB ++ "send_message([], OpCode." ++
                            string:to_upper(FunStr) ++ ")\n" ++
                            ?TAB ++ "if debug:\n" ++
                            ?TAB(2) ++ "print('[INFO] Sent a " ++
                            FunStr ++
                            " packet')\n\n";
                    _ ->
                        Fields = field_info({Encoder, ClientMsg}),
                        FieldStr = fields_to_str(Fields),
                        EncStr = string:titlecase(atom_to_list(Encoder)),
                        "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
                            ?TAB ++ "var m = " ++ EncStr ++ "." ++
                            atom_to_list(ClientMsg) ++
                            ".new()\n" ++
                            set_parameters(Fields, Encoder) ++
                            ?TAB ++ "var payload = m.to_bytes()\n" ++
                            ?TAB ++ "send_message(payload, OpCode." ++
                            string:to_upper(FunStr) ++
                            ")\n" ++
                            ?TAB ++ "if debug:\n" ++
                            ?TAB(2) ++ "print('[INFO] Sent a " ++
                            FunStr ++
                            " packet')\n\n"
                end,
            generate_marshall(Rest, [Op | St0])
    end.

set_parameters(Fields, Encoder) ->
    set_parameters(Fields, Encoder, []).
set_parameters([], _Encoder, St0) ->
    St0;
set_parameters([{F, T, O} | Rest], Encoder, St0) ->
    Var = atom_to_list(F),
    St1 =
        case O of
            required ->
                case T of
                    {msg, MsgType} ->
                        % Message has a complex nested type
                        St0 ++ ?TAB ++ "var n = m.new_" ++
                            atom_to_list(F) ++ "()\n" ++
                            marshall_submsg(
                                "n", MsgType, atom_to_list(F), Encoder, 1
                            );
                    _ ->
                        St0 ++ ?TAB ++ "m.set_" ++ Var ++ "(" ++ Var ++
                            ")\n"
                end;
            repeated ->
                % if the type is simple and the object is repeated, you want to
                % use the "add_" construct rather than "set_"
                case T of
                    {msg, MsgType} ->
                        % Message has complex nested types
                        St0 ++ ?TAB ++ "for item in " ++ Var ++ ":\n" ++
                            ?TAB(2) ++ "var a = m.add_" ++ Var ++
                            "()\n" ++
                            marshall_submsg(
                                "a", MsgType, "item", Encoder, 2
                            );
                    _ ->
                        % Message is some well understood type
                        St0 ++ ?TAB ++ "for item in " ++ Var ++ ":" ++
                            ?TAB(2) ++ "m.add_" ++ Var ++ "(item)\n"
                end;
            optional ->
                St0 ++ ?TAB ++ "if " ++ Var ++ ":\n" ++
                    ?TAB(2) ++ "m.set_" ++ Var ++ "(" ++ Var ++ ")\n"
        end,
    set_parameters(Rest, Encoder, St1).

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
                Name ++ " = null"
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
    dict_fields_to_str(List, "").
dict_fields_to_str([], Acc) ->
    Acc;
dict_fields_to_str([{N, _T, _O} | Tail], "") ->
    Acc1 = "d['" ++ atom_to_list(N) ++ "']",
    dict_fields_to_str(Tail, Acc1);
dict_fields_to_str([{N, _T, _O} | Tail], Acc) ->
    Name = atom_to_list(N),
    Acc1 = Acc ++ "," ++ "d['" ++ Name ++ "']",
    dict_fields_to_str(Tail, Acc1).

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

% TODO
%field_requirements({ProtoLib, ProtoMsg}) ->
%    Defs = erlang:apply(ProtoLib, fetch_msg_def, [ProtoMsg]),
%    field_requirements(Defs, []).
%
%field_requirements([], Acc) ->
%    Acc;
%field_requirements([H|T], Acc) ->
%    Name = maps:get(name, H),
%    Occurrence = maps:get(occurrence, H),
%    Acc1 = [{Name, Occurrence} | Acc],
%    field_requirements(T, Acc1).
%

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
    unmarshall_var(field_info({ProtoLib, ProtoMsg}), ProtoMsg, ProtoLib, []).
unmarshall_var([], _ProtoMsg, _ProtoLib, Acc) ->
    Acc;
unmarshall_var([{_F, _T, _O} | _Rest], ProtoMsg, ProtoLib, Acc) ->
    V = ?TAB ++ "var d = get_" ++ atom_to_list(ProtoMsg) ++ "(m)\n",
    unmarshall_var([], ProtoMsg, ProtoLib, Acc ++ V).

opcode_name_string(OpInfo) ->
    OpCode = gremlin_rpc:opcode(OpInfo),
    case gremlin_rpc:c2s_handler(OpInfo) of
        {_M, F, _A} ->
            atom_to_list(F);
        undefined ->
            % Try the next best guess
            case gremlin_rpc:s2c_call(OpInfo) of
                undefined -> "undefined_" ++ integer_to_list(OpCode);
                Call -> atom_to_list(Call)
            end
    end.

rpc_name(OpInfo) ->
    OpCode = gremlin_rpc:opcode(OpInfo),
    case gremlin_rpc:c2s_handler(OpInfo) of
        {_M, F, _A} ->
            F;
        undefined ->
            % Try the next best guess
            case gremlin_rpc:s2c_call(OpInfo) of
                undefined -> "undefined_" ++ integer_to_list(OpCode);
                Call -> Call
            end
    end.

%submsg_to_gd_dict(MessageType, Encoder) ->
%    submsg_to_gd_dict(MessageType, "item", Encoder).
%submsg_to_gd_dict(MessageType, Prefix, Encoder) ->
%    Fields = field_info({Encoder, MessageType}),
%    %KeyVals = [field_to_var_decl(Prefix,F) || {F, _T, _O} <- Fields],
%    KeyVals = expand_submsgs(Fields, Prefix, Encoder, []),
%    "{ " ++ KeyVals ++ "}".

%expand_submsgs([], _Prefix, _Encoder, Acc) ->
%    Acc;
%expand_submsgs([{F, {msg, vector2}, _O} | Rest], Prefix, Encoder, Acc) ->
%    Acc1 = 
%        Acc ++ "'" ++ atom_to_list(F) ++ "': " ++ "to_vector2_array(" ++
%        Prefix ++ ".get_" ++ atom_to_list(F) ++ "()" ++ "), ",
%    expand_submsgs(Rest, Prefix, Encoder, Acc1);
%expand_submsgs([{F, {msg, SubmsgType}, _O} | Rest], Prefix, Encoder, Acc) ->
%    Acc1 = 
%        Acc ++ "'" ++ atom_to_list(F) ++ "': " ++
%            submsg_to_gd_dict(
%                SubmsgType,
%                Prefix ++ ".get_" ++ atom_to_list(F) ++ "()",
%                Encoder
%            ) ++ ", ",
%    expand_submsgs(Rest, Prefix, Encoder, Acc1);
%expand_submsgs([{F, _T, _O} | Rest], Prefix, Encoder, Acc) ->
%    Acc1 = Acc ++ field_to_var_decl(Prefix, F),
%    expand_submsgs(Rest, Prefix, Encoder, Acc1).
%
%field_to_var_decl(Prefix, F) ->
%    "'" ++ atom_to_list(F) ++ "': " ++ Prefix ++ ".get_" ++ atom_to_list(F) ++
%        "(), ".

marshall_submsg(Var, MessageType, Prefix, Encoder, IndentLevel) ->
    Fields = field_info({Encoder, MessageType}),
    [field_to_set(Var, Prefix, F, IndentLevel) || {F, _T, _O} <- Fields].

field_to_set(Var, Prefix, Field, IndentLevel) ->
    F = atom_to_list(Field),
    ?TAB(IndentLevel) ++ Var ++ ".set_" ++ F ++ "(" ++ Prefix ++ "['" ++ F ++
        "'])\n".

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

%encode_submsg(Encoder, MessageType, [Args]) ->
%    Fields = field_in
%    ?TAB ++ "var s = " ++ "m.new_" ++ atom_to_list(MessageType) ++ "()\n"  ++
%    ?TAB ++ "encode_" ++ atom_to_list(MessageType) ++ "(" ++ untyped_fields_to_str( ++ ")\n"
