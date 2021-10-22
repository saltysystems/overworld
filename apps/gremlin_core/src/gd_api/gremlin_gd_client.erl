-module(gremlin_gd_client).

-export([
    print/0,
    generate_opcodes/2,
    generate_router/2,
    generate_router/3,
    generate_unmarshall/2
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
        gremlin_protocol:op_info(X)
     || X <- gremlin_protocol:registered_ops()
    ],
    St = generate_header(),
    St0 = St ++ ?COMMENT("Signals"),
    St1 = generate_signals(R, St0) ++ "~n~n",
    St2 = St1 ++ ?COMMENT("OpCodes"),
    St3 = generate_opcodes(R, St2),
    St4 = St3 ++ "~n~n" ++ ?COMMENT("Router"),
    St5 = generate_router(R, St4) ++ "~n~n",
    St6 = St5 ++ ?COMMENT("Payload unmarshalling (server packets)"),
    St7 = generate_unmarshall(R, St6),
    St8 = St7 ++ ?COMMENT("Payload marshalling (client packets)"),
    St9 = generate_marshall(R, St8).

generate_header() ->
    "extends Node~n~n" ++
        "class_name GremlinClient~n~n" ++
        "const Proto = preload(\"res://scripts/gremlin_pb.gd\")~n~n" ++
        "const TICKRATE = 6~n~n" ++
        "var ws = WebSocketClient.new()~n~n" ++
        "var timer = 0~n~n" ++
        "export(String) var global_player~n~n".

generate_signals(OpInfo, St0) ->
    generate_signals(OpInfo, [], St0).

generate_signals([], _Seen, St0) ->
    St0;
generate_signals([OpInfo | Rest], SignalsSeen0, St0) ->
    MFA = gremlin_rpc:mfa(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    ServerMsg = gremlin_rpc:server_msg(OpInfo),
    ClientMsg = gremlin_rpc:client_msg(OpInfo),
    OpCode = gremlin_rpc:opcode(OpInfo),
    case lists:member(OpCode, SignalsSeen0) of
        true ->
            generate_signals(Rest, SignalsSeen0, St0);
        false ->
            St1 = next_signal(MFA, Encoder, ServerMsg, ClientMsg, St0),
            SignalsSeen1 = [OpCode | SignalsSeen0],
            generate_signals(Rest, SignalsSeen1, St1)
    end.

next_signal(_MFA, Encoder, ServerMsg, ClientMsg, St0) when
    Encoder == undefined; ServerMsg == undefined; ClientMsg == undefined
->
    St0;
next_signal(_MFA, Encoder, ServerMsg, _ClientMsg, St0) ->
    Fields =
        "(" ++ fields_to_str(field_names({Encoder, ServerMsg})) ++
            ")",
    Signal = "signal " ++ atom_to_list(ServerMsg) ++ Fields ++ "~n",
    St0 ++ Signal.

generate_opcodes(Ops, St0) ->
    BytePack =
        "func bytepack(opcode):~n" ++
            ?TAB ++ "var buf = StreamPeerBuffer.new()~n" ++
            ?TAB ++ "buf.big_endian=true~n" ++
            ?TAB ++ "buf.put_16(opcode)~n" ++
            ?TAB ++ "return buf.data_array~n" ++ "~n",
    Fun = "var OpCode = {~n",
    St1 = next_opcode(Ops, ok, St0 ++ BytePack ++ Fun),
    St1 ++ "}~n~n".
next_opcode([], ok, St0) ->
    St0;
next_opcode([OpInfo | Rest], ok, St0) ->
    OpCode = gremlin_rpc:opcode(OpInfo),
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    OpString = io_lib:format("~p", [OpCode]),
    Op =
        ?TAB ++ string:to_upper(atom_to_list(F)) ++
            " = " ++ "bytepack(" ++ OpString ++ "),~n",
    next_opcode(Rest, ok, St0 ++ Op).

generate_router(Operation, St0) ->
    Preamble =
        "func route(packet):~n" ++
            ?TAB ++ "var opcode = packet.subarray(0,1)~n" ++
            ?TAB ++ "var payload = []~n" ++
            ?TAB ++ "if packet.size() > 2:~n" ++
            ?TAB ++ ?TAB ++ "payload = packet.subarray(2,-1)~n" ++
            ?TAB ++ "match opcode:~n",
    St1 = generate_router(Operation, [], St0 ++ Preamble),
    St1 ++ ?TAB ++ ?TAB ++ "_:~n" ++
        ?TAB ++ ?TAB ++ ?TAB ++
        "print(\"[WARNING]\ Got an unknown opcode from the server: \" + opcode)~n".
generate_router([], Routes, St0) ->
    St0 ++ Routes;
generate_router([OpInfo | Rest], Routes, St0) ->
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    Op =
        ?TAB ++ ?TAB ++ "OpCode." ++ string:to_upper(atom_to_list(F)) ++
            ":~n" ++
            ?TAB ++ ?TAB ++ ?TAB ++ atom_to_list(F) ++ "(payload)~n",
    generate_router(Rest, Routes, St0 ++ Op).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall(
    [OpInfo | Rest], St0
) ->
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    ServerMsg = gremlin_rpc:server_msg(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    case ServerMsg of
        undefined ->
            % No message to unpack
            Op =
                "func " ++ atom_to_list(F) ++ "(packet):~n" ++
                    ?TAB ++ "print(\"[INFO] Received a " ++
                    atom_to_list(F) ++ " packet (no-op)\")~n~n",
            generate_unmarshall(Rest, St0 ++ Op);
        _ ->
            FunStr = atom_to_list(F),
            Op =
                "func " ++ FunStr ++ "(packet):~n" ++
                    ?TAB ++ "print(\"[INFO] Processing a " ++ FunStr ++
                    " packet\")~n" ++
                    ?TAB ++ "var m = Proto." ++ atom_to_list(ServerMsg) ++
                    ".new()~n" ++
                    ?TAB ++ "if result_code != Proto.PB_ERR.NO_ERRORS:~n" ++
                    ?TAB ++ ?TAB ++ "print(\"[CRITICAL] Error decoding new " ++
                    FunStr ++ " packet\")~n" ++
                    ?TAB ++ ?TAB ++ "return~n",
            Vars = unmarshall_var({Encoder, ServerMsg}),
            Signal =
                ?TAB ++ "emit_signal(\"" ++ FunStr ++ "\"," ++
                    fields_to_str(field_names({Encoder, ServerMsg})) ++
                    "\)~n~n",
            generate_unmarshall(Rest, St0 ++ Op ++ Vars ++ Signal)
    end.

generate_marshall([], St0) ->
    St0;
generate_marshall(
    [OpInfo | Rest], St0
) ->
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    ClientMsg = gremlin_rpc:client_msg(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    case ClientMsg of
        undefined ->
            % No message to pack
            generate_marshall(Rest, St0);
        _ ->
            FunStr = atom_to_list(F),
            Fields = field_names({Encoder, ClientMsg}),
            FieldStr = fields_to_str(Fields),
            Op =
                "func " ++ FunStr ++ "(" ++ FieldStr ++ "):~n" ++
                    ?TAB ++ "var m = Proto." ++ atom_to_list(ClientMsg) ++
                    ".new()~n" ++
                    set_parameters(Fields) ++
                    ?TAB ++ "var payload = m.to_bytes()~n" ++
                    ?TAB ++ "send_message(payload, OpCode." ++ FunStr ++
                    ")~n" ++
                    ?TAB ++ "print(\"[INFO] Sent a " ++ FunStr ++
                    " packet\")~n~n",
            generate_marshall(Rest, St0 ++ Op)
    end.

set_parameters(Fields) ->
    set_parameters(Fields, []).
set_parameters([], St0) ->
    St0;
set_parameters([H | T], St0) ->
    Var = atom_to_list(H),
    St1 = St0 ++ ?TAB ++ "var m.set_" ++ Var ++ "(" ++ Var ++ ")~n",
    set_parameters(T, St1).

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
