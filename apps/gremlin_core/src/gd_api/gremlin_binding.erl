-module(gremlin_binding).

% this module is heavily iterated upon and is in its 2nd major version.
% it'll need a third major version to be really solid :)

-export([
    write/0,
    print/0
]).

-define(TAB, [9]).
-define(TAB(N), lists:foldl(fun(_N, Acc) -> [9] ++ Acc end, [], lists:seq(0,N - 1))).
-define(DEFAULT_ENCODER, gremlin_pb).

write() ->
    file:write_file(
        "apps/gremlin_core/static/libgremlin.gd", gremlin_binding:print()
    ).

print() ->
    Ops = [
        gremlin_protocol:op_info(X)
     || X <- gremlin_protocol:registered_ops()
    ],
    Preloads = load_scripts(Ops),
    Enums = generate_enums(Ops),
    Signals = generate_signals(Ops, []),
    Opcodes = generate_opcodes(Ops, []),
    Router = generate_router(Ops, []),
    Unmarshall = generate_unmarshall(Ops, []),
    Marshall = generate_marshall(Ops, []),
    Map = #{
        "preloads" => Preloads,
        "constants" => Enums,
        "signals" => Signals,
        "opcodes" => Opcodes,
        "router" => Router,
        "unmarshall" => Unmarshall,
        "marshall" => Marshall
    },
    T = bbmustache:parse_file(
        "apps/gremlin_core/templates/libgremlin.mustache"
    ),
    bbmustache:compile(T, Map).

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
            ?TAB ++ ?TAB ++ ?TAB ++ "server_" ++ OpName ++
            "(payload)",
    generate_router(Rest, Routes, [Op | St0]).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall([OpInfo | Rest], St0) ->
    ServerMsg = gremlin_rpc:s2c_call(OpInfo),
    Encoder = correct_encoder(gremlin_rpc:encoder(OpInfo), ServerMsg),
    FunStr = opcode_name_string(OpInfo),
    write_function(ServerMsg, FunStr, Encoder, Rest, St0).

write_function(undefined, undefined, _Encoder, Rest, St0) ->
    % No message to unpack, no sensible name to decode. Assume this is a
    % message only meant to be *sent* to the server
    generate_unmarshall(Rest, St0);
write_function(undefined, FunStr, _Encoder, Rest, St0) ->
    % In this case, there's a named function but no servermsg. We can safely
    % assume that there's simply no arguments for this fun.
    Op =
        "func " ++ "server_" ++ FunStr ++ "(_packet):\n" ++
            ?TAB ++ "print('[WARN] Received a " ++
            FunStr ++ " packet')\n",
    % We don't emit a signal for these because I'm not quite sure what
    % to do with messageless packets from the server.
    %++ ?TAB ++ "emit_signal('" ++ FunStr ++ "')\n\n",
    generate_unmarshall(Rest, [Op | St0]);
write_function(ServerMsg, FunStr, Encoder, Rest, St0) ->
    EncStr = string:titlecase(atom_to_list(Encoder)),
    Op =
        "func " ++ "server_" ++ FunStr ++ "(packet):\n" ++
            ?TAB ++ "if debug:\n" ++
            ?TAB ++ ?TAB ++ "print('[DEBUG] Processing a " ++ FunStr ++
            " packet')\n" ++
            ?TAB ++ "var m = " ++ EncStr ++ "." ++ atom_to_list(ServerMsg) ++
            ".new()\n" ++
            ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
            ?TAB ++ "if result_code != " ++ EncStr ++
            ".PB_ERR.NO_ERRORS:\n" ++
            ?TAB ++ ?TAB ++ "print('[CRITICAL] Error decoding new " ++
            FunStr ++ " packet')\n" ++
            ?TAB ++ ?TAB ++ "return\n",
    Vars = unmarshall_var({Encoder, ServerMsg}),
    Signal =
        ?TAB ++ "emit_signal('" ++ atom_to_list(ServerMsg) ++ "'," ++
            untyped_fields_to_str(field_info({Encoder, ServerMsg})) ++
            "\)\n\n",
    generate_unmarshall(Rest, [Op ++ Vars ++ Signal | St0]).

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
                            ?TAB ++ ?TAB ++ "print('[INFO] Sent a " ++
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
                            ?TAB ++ ?TAB ++ "print('[INFO] Sent a " ++
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
                        St0 ++ ?TAB ++ "var n = m.new_" ++ atom_to_list(MsgType) ++ "()\n" ++
                        marshall_submsg("n", MsgType, atom_to_list(F), Encoder, 1);
                    _ ->
                        St0 ++ ?TAB ++ "m.set_" ++ Var ++ "(" ++ Var ++ ")\n"
                end;
            repeated ->
                % if the type is simple and the object is repeated, you want to
                % use the "add_" construct rather than "set_"
                case T of
                    {msg, MsgType} ->
                        % Message has complex nested types
                        St0 ++ ?TAB ++ "for item in " ++ Var ++ ":\n" ++
                            ?TAB ++ ?TAB ++ "var a = m.add_" ++ Var ++
                            "()\n" ++
                            marshall_submsg("a", MsgType, "item", Encoder, 2);
                    _ ->
                        % Message is some well understood type
                        St0 ++ ?TAB ++ "for item in " ++ Var ++ ":" ++
                            ?TAB ++ ?TAB ++ "m.add_" ++ Var ++ "(item)\n"
                end;
            optional ->
                St0 ++ ?TAB ++ "if " ++ Var ++ ":\n" ++
                    ?TAB ++ ?TAB ++ "m.set_" ++ Var ++ "(" ++ Var ++ ")\n"
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
                Name ++ "= null, " ++ Acc
        end,
    fields_to_str(Tail, Acc1).

% this test is broken
%fields_to_str_test() ->
%    Fields = [
%        {string, string},
%        {float, float},
%        {int32, int32},
%        {int64, int64},
%        {bytes, bytes}
%    ],
%    fields_to_str(Fields).

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
    unmarshall_var(field_info({ProtoLib, ProtoMsg}), ProtoLib, []).
unmarshall_var([], _ProtoLib, Acc) ->
    Acc;
unmarshall_var([{F, T, O} | Rest], ProtoLib, Acc) ->
    V =
        % First check to see if the message is a repeated one or not
        case O of
            repeated ->
                % We need to iterate through every message. If the repeated
                % object is a well understood type, Godobuf will automatically
                % create an array with the repeated type.
                % Now determine there's a submessage to unpack, or just repeated items
                case T of
                    {msg, MessageType} ->
                        % The message is made of some complex submsg. Out of
                        % laziness we don't recurse the type detection and just
                        % assume messages won't be more complex without the
                        % author having to rewrite the code :)
                        ?TAB ++ "var " ++ atom_to_list(F) ++ " = []\n" ++
                            ?TAB ++ "for item in m.get_" ++ atom_to_list(F) ++
                            "():\n" ++
                            ?TAB ++ ?TAB ++ "var dict = " ++
                            submsg_to_gd_dict(MessageType, ProtoLib) ++ "\n" ++
                            ?TAB ++ ?TAB ++ atom_to_list(F) ++
                            ".append(dict)\n";
                    _ ->
                        ?TAB ++ "var " ++ atom_to_list(F) ++ " = " ++
                            "m.get_" ++
                            atom_to_list(F) ++ "()\n"
                end;
            _ ->
                case T of 
                    {msg, MessageType} ->
                        Submsg = atom_to_list(F) ++ "_msg",
                        ?TAB ++ "var " ++ atom_to_list(F) ++ "_msg = m.get_" ++ Submsg ++ "()\n" ++
                        ?TAB ++ "var " ++ atom_to_list(F) ++ " = " ++ submsg_to_gd_dict(MessageType, Submsg, ProtoLib) ++ "\n";
                    _ -> 
                        ?TAB ++ "var " ++ atom_to_list(F) ++ " = " ++ "m.get_" ++
                            atom_to_list(F) ++ "()\n"
                end
        end,
    unmarshall_var(Rest, ProtoLib, Acc ++ V).

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

submsg_to_gd_dict(MessageType, Encoder) ->
    submsg_to_gd_dict(MessageType, "item", Encoder).
submsg_to_gd_dict(MessageType, Prefix, Encoder) ->
    Fields = field_info({Encoder, MessageType}),
    %KeyVals = [field_to_var_decl(Prefix,F) || {F, _T, _O} <- Fields],
    KeyVals = expand_submsgs(Fields, Prefix, Encoder, []),
    "{ " ++ KeyVals ++ "}".

expand_submsgs([], _Prefix, _Encoder, Acc) ->
    Acc;
expand_submsgs([{F, {msg, SubmsgType}, _O} | Rest], Prefix, Encoder, Acc) ->
    Acc1 = Acc ++ "'" ++ atom_to_list(F) ++ "': " ++ submsg_to_gd_dict(SubmsgType, atom_to_list(F) ++ "_obj", Encoder) ++ ", " ,
    expand_submsgs(Rest, Prefix, Encoder, Acc1);
expand_submsgs([{F, _T, _O} | Rest], Prefix, Encoder, Acc) ->
    Acc1 = Acc ++ field_to_var_decl(Prefix, F),
    expand_submsgs(Rest, Prefix, Encoder, Acc1).

field_to_var_decl(Prefix,F) ->
    "'" ++ atom_to_list(F) ++ "': " ++ Prefix ++ ".get_" ++ atom_to_list(F) ++ "(), ".

marshall_submsg(Var, MessageType, Prefix, Encoder, IndentLevel) ->
    Fields = field_info({Encoder, MessageType}),
    [field_to_set(Var, Prefix, F, IndentLevel) || {F, _T, _O} <- Fields].

field_to_set(Var, Prefix, Field, IndentLevel) ->
    F = atom_to_list(Field),
    ?TAB(IndentLevel) ++ Var ++ ".set_" ++ F ++ "(" ++ Prefix ++ "['" ++ F ++ "'])\n".

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
