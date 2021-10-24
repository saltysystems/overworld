-module(gremlin_binding).

-export([
    print/0,
    fields_to_str_test/0
]).

-define(TAB, [9]).

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
    T = bbmustache:parse_file("apps/gremlin_core/templates/libgremlin.mustache"),
    bbmustache:compile(T, Map).


load_scripts(Ops) ->
    load_scripts(Ops, [], []).
load_scripts([], _Seen, Acc) -> lists:reverse(Acc);
load_scripts([H|T], Seen, Acc) ->
    case gremlin_rpc:encoder(H) of
        undefined -> load_scripts(T, Seen, Acc);
        Encoder -> 
            case lists:member(Encoder, Seen) of 
                false ->
                    Const = "const " ++ string:titlecase(atom_to_list(Encoder)) ++ " = preload('scripts/",
                    Script = atom_to_list(Encoder) ++ ".gd')",
                    Seen1 = [ Encoder | Seen ],
                    load_scripts(T, Seen1, [Const ++ Script | Acc ]);
                true -> load_scripts(T, Seen, Acc)
            end
    end.

generate_signals(OpInfo, St0) ->
    generate_signals(OpInfo, [], St0).

generate_signals([], _Seen, St0) ->
    lists:reverse(St0);
generate_signals([OpInfo | Rest], SignalsSeen0, St0) ->
    MFA = gremlin_rpc:mfa(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    ServerMsg = gremlin_rpc:server_msg(OpInfo),
    case lists:member(ServerMsg, SignalsSeen0) of
        true ->
            generate_signals(Rest, SignalsSeen0, St0);
        false ->
            St1 = next_signal(MFA, Encoder, ServerMsg, St0),
            SignalsSeen1 = [ServerMsg | SignalsSeen0],
            generate_signals(Rest, SignalsSeen1, St1)
    end.

next_signal(_MFA, Encoder, ServerMsg, St0) when
    Encoder == undefined; ServerMsg == undefined ->
    St0;
next_signal(_MFA, Encoder, ServerMsg, St0) ->
    Fields =
        "(" ++ untyped_fields_to_str(field_names({Encoder, ServerMsg})) ++
            ")",
    Signal = "signal " ++ atom_to_list(ServerMsg) ++ Fields,
    [ Signal | St0 ].

generate_opcodes(Ops, St0) ->
    next_opcode(Ops, ok, St0).

next_opcode([], ok, St0) ->
    lists:reverse(St0);
next_opcode([OpInfo | Rest], ok, St0) ->
    OpCode = gremlin_rpc:opcode(OpInfo),
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    OpString = io_lib:format("~p", [OpCode]),
    Op =
        string:to_upper(atom_to_list(F)) ++
            " = " ++ "bytepack(" ++ OpString ++ "),",
    next_opcode(Rest, ok, [Op | St0]).

generate_router(Operation, St0) ->
    generate_router(Operation, [], St0).

generate_router([], Routes, St0) ->
    St0 ++ Routes;
generate_router([OpInfo | Rest], Routes, St0) ->
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    Op =
        "OpCode." ++ string:to_upper(atom_to_list(F)) ++ ":\n" ++
        ?TAB ++ ?TAB ++ ?TAB ++ "server_" ++ atom_to_list(F) ++ "(payload)",
    generate_router(Rest, Routes, [Op | St0]).

generate_unmarshall([], St0) ->
    St0;
generate_unmarshall([OpInfo | Rest], St0) ->
    {_M, F, _A} = gremlin_rpc:mfa(OpInfo),
    ServerMsg = gremlin_rpc:server_msg(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    FunStr = atom_to_list(F),
    case ServerMsg of
        undefined ->
            % No message to unpack
            Op =
                "func " ++ "server_" ++ FunStr ++ "(packet):\n" ++
                    ?TAB ++ "print('[INFO] Received a " ++
                    FunStr ++ " packet (no-op)')",
            generate_unmarshall(Rest, [ Op | St0 ]);
        _ ->
            EncStr = string:titlecase(atom_to_list(Encoder)),
            Op =
                "func " ++ "server_" ++ FunStr ++ "(packet):\n" ++
                    ?TAB ++ "print('[INFO] Processing a " ++ FunStr ++
                    " packet')\n" ++
                    ?TAB ++ "var m = " ++ EncStr ++ "." ++ atom_to_list(ServerMsg) ++
                    ".new()\n" ++
                    ?TAB ++ "if result_code != " ++ EncStr ++ ".PB_ERR.NO_ERRORS:\n" ++
                    ?TAB ++ ?TAB ++ "print('[CRITICAL] Error decoding new " ++
                    FunStr ++ " packet')\n" ++
                    ?TAB ++ ?TAB ++ "return\n",
            Vars = unmarshall_var({Encoder, ServerMsg}),
            Signal =
                ?TAB ++ "emit_signal('" ++ FunStr ++ "'," ++
                    untyped_fields_to_str(field_names({Encoder, ServerMsg})) ++
                    "\)\n\n",
            generate_unmarshall(Rest, [ Op ++ Vars ++ Signal | St0 ] )
    end.

generate_marshall([], St0) ->
    lists:reverse(St0);
generate_marshall(
    [OpInfo | Rest], St0
) ->
    {_M, Fun, _A} = gremlin_rpc:mfa(OpInfo),
    ClientMsg = gremlin_rpc:client_msg(OpInfo),
    Encoder = gremlin_rpc:encoder(OpInfo),
    case ClientMsg of
        undefined ->
            % No message to pack
            generate_marshall(Rest, St0);
        _ ->
            FunStr = atom_to_list(Fun),
            Fields = field_names({Encoder, ClientMsg}),
            FieldNames = [ F || {F,_T} <- Fields ],
            FieldStr = fields_to_str(Fields),
            EncStr = string:titlecase(atom_to_list(Encoder)),
            Op =
                "func " ++ FunStr ++ "(" ++ FieldStr ++ "):\n" ++
                    ?TAB ++ "var m = " ++ EncStr ++ "." ++ atom_to_list(ClientMsg) ++
                    ".new()\n" ++
                    set_parameters(FieldNames) ++
                    ?TAB ++ "var payload = m.to_bytes()\n" ++
                    ?TAB ++ "send_message(payload, OpCode." ++ string:to_upper(FunStr) ++
                    ")\n" ++
                    ?TAB ++ "print('[INFO] Sent a " ++ FunStr ++
                    " packet')\n\n",
            generate_marshall(Rest, [ Op | St0 ])
    end.

set_parameters(Fields) ->
    set_parameters(Fields, []).
set_parameters([], St0) ->
    St0;
set_parameters([H | T], St0) ->
    Var = atom_to_list(H),
    St1 = St0 ++ ?TAB ++ "var m.set_" ++ Var ++ "(" ++ Var ++ ")\n",
    set_parameters(T, St1).

fields_to_str(List) ->
    fields_to_str(List, "").
fields_to_str([], Acc) ->
    Acc;
fields_to_str([{N,T} | Tail], "") ->
    Name = atom_to_list(N),
    Acc1 = case T of
               {enum, _} ->
                    Name;
               Type ->
                    Name ++ ": " ++ atom_to_list(Type)
           end,
    fields_to_str(Tail, Acc1);
fields_to_str([{N,T} | Tail], Acc) ->
    Name = atom_to_list(N),
    Acc1 = case T of
               {enum, _} ->
                    Acc ++ ", " ++ Name;
               Type ->
                    Acc ++ ", " ++ Name ++ ": " ++ atom_to_list(Type)
           end,
    fields_to_str(Tail, Acc1).

fields_to_str_test() ->
    Fields = [{string, string},
              {float, float},
              {int32, int32},
              {int64, int64},
              {bytes, bytes},
              {enum, {enum, something}}],
    fields_to_str(Fields).

untyped_fields_to_str(List) ->
    untyped_fields_to_str(List, "").
untyped_fields_to_str([], Acc) ->
    Acc;
untyped_fields_to_str([{N,_T} | Tail], "") ->
    Acc1 = atom_to_list(N),
    untyped_fields_to_str(Tail, Acc1);
untyped_fields_to_str([{N,_T} | Tail], Acc) ->
    Name = atom_to_list(N),
    Acc1 = Acc ++ "," ++ Name,
    untyped_fields_to_str(Tail, Acc1).

field_names({ProtoLib, ProtoMsg}) ->
    Defs = erlang:apply(ProtoLib, fetch_msg_def, [ProtoMsg]),
    field_names(Defs, []).

field_names([], Acc) ->
    Acc;
field_names([H | T], Acc) ->
    Name = maps:get(name, H),
    Type = maps:get(type, H),
    Acc1 = [{Name, Type} | Acc],
    field_names(T, Acc1).

unmarshall_var({ProtoLib, ProtoMsg}) ->
    %{Fields,_Types} = field_names({ProtoLib, ProtoMsg}),
    Fields = [ F || {F,_T} <- field_names({ProtoLib, ProtoMsg}) ],
    unmarshall_var(Fields, []).
unmarshall_var([], Acc) ->
    Acc;
unmarshall_var([H | T], Acc) ->
    V =
        ?TAB ++ "var " ++ atom_to_list(H) ++ " = " ++ "m.get_" ++
            atom_to_list(H) ++ "()\n",
    unmarshall_var(T, Acc ++ V).
