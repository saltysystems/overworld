-module(gremlin_binding).

-export([
    write/0,
    print/0,
    fields_to_str_test/0
]).

-define(TAB, [9]).

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
    Signals = generate_signals(Ops, []),
    Opcodes = generate_opcodes(Ops, []),
    Router = generate_router(Ops, []),
    Unmarshall = generate_unmarshall(Ops, []),
    Marshall = generate_marshall(Ops, []),
    Map = #{
        "preloads" => Preloads,
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

load_scripts(Ops) ->
    load_scripts(Ops, [], []).
load_scripts([], _Seen, Acc) ->
    lists:reverse(Acc);
load_scripts([H | T], Seen, Acc) ->
    io:format("Op is: ~p~n", [H]),
    case gremlin_rpc:encoder(H) of
        undefined ->
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
    io:format("processing: ~p~n", [OpInfo]),
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
    Encoder == undefined; MsgFromServer == undefined ->
    % If there's no msg to be decoded, there's no relevant signal to send.
    % TODO: verify this
    %Signal = "signal " ++ atom_to_list(MsgFromServer),
    St0;
next_signal(MsgFromServer, Encoder, St0) ->
    Fields =
        "(" ++ untyped_fields_to_str(field_info({Encoder, MsgFromServer})) ++
            ")",
    Signal = "signal " ++ atom_to_list(MsgFromServer) ++ Fields,
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
        string:to_upper(OpName) ++ " = " 
        ++ "bytepack(" ++ OpString ++ "),",
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
    Encoder = gremlin_rpc:encoder(OpInfo),
    FunStr = opcode_name_string(OpInfo),
    write_function(ServerMsg, FunStr, Encoder, Rest, St0).

write_function(undefined, undefined, _Encoder, Rest, St0 ) ->
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
            ?TAB ++ "print('[INFO] Processing a " ++ FunStr ++
            " packet')\n" ++
            ?TAB ++ "var m = " ++ EncStr ++ "." ++
            atom_to_list(ServerMsg) ++
            ".new()\n" ++
            ?TAB ++ "var result_code = m.from_bytes(packet)\n" ++
            ?TAB ++ "if result_code != " ++ EncStr ++
            ".PB_ERR.NO_ERRORS:\n" ++
            ?TAB ++ ?TAB ++ "print('[CRITICAL] Error decoding new " ++
            FunStr ++ " packet')\n" ++
            ?TAB ++ ?TAB ++ "return\n",
    Vars = unmarshall_var({Encoder, ServerMsg}),
    Signal =
        ?TAB ++ "emit_signal('" ++ FunStr ++ "'," ++
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
                            ?TAB ++ "print('[INFO] Sent a " ++ FunStr ++
                            " packet')\n\n";
                    _ ->
                        Fields = field_info({Encoder, ClientMsg}),
                        FieldStr = fields_to_str(Fields),
                        EncStr = string:titlecase(atom_to_list(Encoder)),
                        "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
                            ?TAB ++ "var m = " ++ EncStr ++ "." ++
                            atom_to_list(ClientMsg) ++
                            ".new()\n" ++
                            set_parameters(Fields) ++
                            ?TAB ++ "var payload = m.to_bytes()\n" ++
                            ?TAB ++ "send_message(payload, OpCode." ++
                            string:to_upper(FunStr) ++
                            ")\n" ++
                            ?TAB ++ "print('[INFO] Sent a " ++ FunStr ++
                            " packet')\n\n"
                end,
            generate_marshall(Rest, [Op | St0])
    end.

set_parameters(Fields) ->
    set_parameters(Fields, []).
set_parameters([], St0) ->
    St0;
set_parameters([{F,_T,O} | Rest], St0) ->
    Var = atom_to_list(F),
    St1 = case O of 
        required -> 
            St0 ++ ?TAB ++ "m.set_" ++ Var ++ "(" ++ Var ++ ")\n";
        optional ->
            St0 ++ ?TAB ++ "if " ++ Var ++ ":\n" ++ 
                    ?TAB ++ ?TAB ++ "m.set_" ++ Var ++ "(" ++ Var ++ ")\n"
    end,
    set_parameters(Rest, St1).

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
    io:format("end of function: ~p~n", [Acc]),
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
                    uint32 ->
                        Name ++ ": " ++ "int";
                    Type ->
                        Name ++ ": " ++ atom_to_list(Type)
                end;
            optional ->
                % If the parameter is optional, set the parameter to =Null and use no typing
                Name ++ "=Null"
        end,
    io:format("Tail is ~p, Acc1 is ~p~n", [Tail, Acc1]),
    fields_to_str(Tail, Acc1);
fields_to_str([{N, T, O}=H | Tail], Acc) ->
    io:format("Got H: ~p~n", [H]),
    io:format("Got Acc: ~p~n", [Acc]),
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
                    uint32 ->
                        Name ++ ": " ++ "int" ++ ", " ++ Acc;
                    Type ->
                        Name ++ ": " ++ atom_to_list(Type) ++ ", " ++ Acc
                end;
            optional ->
                % If the parameter is optional, set the parameter to =Null and use no typing
                Name ++ "=Null, " ++ Acc
        end,
    fields_to_str(Tail, Acc1).

fields_to_str_test() ->
    Fields = [
        {string, string},
        {float, float},
        {int32, int32},
        {int64, int64},
        {bytes, bytes},
        {enum, {enum, something}}
    ],
    fields_to_str(Fields).

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
    Defs = erlang:apply(ProtoLib, fetch_msg_def, [ProtoMsg]),
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
    unmarshall_var(field_info({ProtoLib, ProtoMsg}), []).
unmarshall_var([], Acc) ->
    Acc;
unmarshall_var([{F,_T,_O} | Rest], Acc) ->
    V =
        ?TAB ++ "var " ++ atom_to_list(F) ++ " = " ++ "m.get_" ++
            atom_to_list(F) ++ "()\n",
    unmarshall_var(Rest, Acc ++ V).


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
