-module(gremlin_gd_client).

-export([
    print/0
]).

-define(COMMENT_BLOCK,
    "###########################################################################~n"
).
-define(COMMENT(Message),
    ?COMMENT_BLOCK ++ "#  " ++ Message ++ "~n" ++ ?COMMENT_BLOCK ++ "~n"
).
-define(TAB, [9]).

print() ->
    R = [
        {X, gremlin_protocol:op_info(X)}
     || X <- gremlin_protocol:registered_ops()
    ],
    St = generate_header(),
    St0 = St ++ ?COMMENT("Signals"),
    St1 = generate_signals(R, [], St0) ++ "~n~n",
    St2 = St1 ++ ?COMMENT("OpCodes"),
    St3 = generate_opcodes(R, St2),
    St4 = St3 ++ "~n~n" ++ ?COMMENT("Router"),
    St5 = generate_router(R, St4) ++ "~n~n",
    St6 = St5 ++ ?COMMENT("Payload unmarshalling"),
    St7 = generate_unmarshall(R, St6).

generate_header() ->
    "extends Node~n~n" ++
        "class_name GremlinClient~n~n" ++
        "const Proto = preload(\"res://scripts/gremlin_pb.gd\")~n~n" ++
        "const TICKRATE = 6~n~n" ++
        "var ws = WebSocketClient.new()~n~n" ++
        "var timer = 0~n~n" ++
        "export(String) var global_player~n~n".

generate_signals([], _SignalSeen, St0) ->
    St0;
generate_signals(
    [{_OpCode, {{_Module, _Fun, _Arity}, {_ProtoLib, none}}} | Rest],
    SignalsSeen,
    St0
) ->
    generate_signals(Rest, SignalsSeen, St0);
generate_signals(
    [{_OpCode, {{_Module, _Fun, _Arity}, {ProtoLib, ProtoMsg}}} | Rest],
    SignalsSeen,
    St0
) ->
    case lists:member(ProtoMsg, SignalsSeen) of
        false ->
            Fields =
                "(" ++ fields_to_str(field_names({ProtoLib, ProtoMsg})) ++
                    ")",
            Signal = "signal " ++ atom_to_list(ProtoMsg) ++ Fields ++ "~n",
            Seen = [ProtoMsg | SignalsSeen],
            generate_signals(Rest, Seen, St0 ++ Signal);
        true ->
            generate_signals(Rest, SignalsSeen, St0)
    end.

generate_opcodes(Ops, St0) ->
    BytePack =
        "func bytepack(opcode):~n" ++
            ?TAB ++ "var buf = StreamPeerBuffer.new()~n" ++
            ?TAB ++ "buf.big_endian=true~n" ++
            ?TAB ++ "buf.put_16(opcode)~n" ++
            ?TAB ++ "return buf.data_array~n" ++ "~n",
    Fun = "var OpCode = {~n",
    generate_opcodes(Ops, [], St0 ++ BytePack ++ Fun).
generate_opcodes([], [], St0) ->
    St0 ++ "}~n~n";
generate_opcodes(
    [{OpCode, {{_M, F, _A}, {_ProtoLib, _ProtoMsg}}} | R], [], St0
) ->
    OpString = io_lib:format("~p", [OpCode]),
    Op =
        ?TAB ++ string:to_upper(atom_to_list(F)) ++
            " = " ++ "bytepack(" ++ OpString ++ "),~n",
    generate_opcodes(R, [], St0 ++ Op).

generate_router(Operation, St0) ->
    Preamble =
        "func route(packet):~n" ++
            ?TAB ++ "var opcode = packet.subarray(0,1)~n" ++
            ?TAB ++ "var payload = []~n" ++
            ?TAB ++ "if packet.size() > 2:~n" ++
            ?TAB ++ ?TAB ++ "payload = packet.subarray(2,-1)~n" ++
            ?TAB ++ "match opcode:~n",
    generate_router(Operation, [], St0 ++ Preamble).
generate_router([], Routes, St0) ->
    St0 ++ Routes;
generate_router(
    [{_OpCode, {{_Module, Fun, _Arity}, {_ProtoLib, _ProtoMsg}}} | Rest],
    Routes,
    St0
) ->
    Op =
        ?TAB ++ ?TAB ++ "OpCode." ++ string:to_upper(atom_to_list(Fun)) ++
            ":~n" ++
            ?TAB ++ ?TAB ++ ?TAB ++ atom_to_list(Fun) ++ "(payload)~n",
    generate_router(Rest, Routes, St0 ++ Op).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall(
    [{_Opcode, {{_Module, Fun, _Arity}, {_ProtoLib, none}}} | Rest], St0
) ->
    Op =
        ?TAB ++ "func " ++ atom_to_list(Fun) ++ "(packet):~n" ++
            ?TAB ++ ?TAB ++ "print(\"[INFO] Received a " ++
            atom_to_list(Fun) ++ " packet (no-op)\")~n~n",
    generate_unmarshall(Rest, St0 ++ Op);
generate_unmarshall(
    [{_Opcode, {{_Module, Fun, _Arity}, {ProtoLib, ProtoMsg}}} | Rest], St0
) ->
    FunStr = atom_to_list(Fun),
    Op =
        ?TAB ++ "func " ++ FunStr ++ "(packet):~n" ++
            ?TAB ++ ?TAB ++ "print(\"[INFO] Processing a " ++ FunStr ++
            " packet\")~n" ++
            ?TAB ++ ?TAB ++ "var m = Proto." ++ atom_to_list(ProtoMsg) ++
            ".new()~n" ++
            ?TAB ++ ?TAB ++ "if result_code != Proto.PB_ERR.NO_ERRORS:~n" ++
            ?TAB ++ ?TAB ++ ?TAB ++ "print(\"[CRITICAL] Error decoding new " ++
            FunStr ++ " packet\")~n" ++
            ?TAB ++ ?TAB ++ ?TAB ++ "return~n",
    Vars = unmarshall_var({ProtoLib, ProtoMsg}),
    Signal =
        ?TAB ++ ?TAB ++ "emit_signal(\"" ++ FunStr ++ "\"," ++
            fields_to_str(field_names({ProtoLib, ProtoMsg})) ++ "\)~n~n",
    generate_unmarshall(Rest, St0 ++ Op ++ Vars ++ Signal).

fields_to_str(List) ->
    fields_to_str(List, "").
fields_to_str([], Acc) ->
    Acc;
fields_to_str([H | T], "") ->
    Acc1 = atom_to_list(H),
    fields_to_str(T, Acc1);
fields_to_str([H | T], Acc) ->
    Acc1 = atom_to_list(H),
    Acc2 = Acc ++ "," ++ Acc1,
    fields_to_str(T, Acc2).

field_names({ProtoLib, ProtoMsg}) ->
    Defs = erlang:apply(ProtoLib, fetch_msg_def, [ProtoMsg]),
    field_names(Defs, []).

field_names([], Acc) ->
    Acc;
field_names([H | T], Acc) ->
    Acc1 = [maps:get(name, H) | Acc],
    field_names(T, Acc1).

unmarshall_var({ProtoLib, ProtoMsg}) ->
    Fields = field_names({ProtoLib, ProtoMsg}),
    unmarshall_var(Fields, []).
unmarshall_var([], Acc) ->
    Acc;
unmarshall_var([H | T], Acc) ->
    V =
        ?TAB ++ ?TAB ++ "var " ++ atom_to_list(H) ++ " = " ++ "m.get_" ++
            atom_to_list(H) ++ "()~n",
    unmarshall_var(T, Acc ++ V).
