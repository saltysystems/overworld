-module(saline_zone_router).
-behaviour(saline_rpc).

% public API
-export([
    join/2,
    part/1,
    action/2,
    state_transfer/2
]).

% saline callbacks
-export([rpc_info/0]).

%%%====================================================================
%%% Saline RPCs
%%%====================================================================
-define(ZONE_JOIN, 16#3005).
-define(ZONE_PART, 16#3006).
-define(ZONE_ACTION, 16#3007).
-define(ZONE_STATE_XFER, 16#3008).

-spec rpc_info() -> saline_rpc:callbacks().
rpc_info() ->
    [
        #{
            opcode => ?ZONE_ACTION,
            c2s_call => action,
            c2s_handler => {?MODULE, action, 2},
            encoder => demo_pb
        },
        #{
            opcode => ?ZONE_JOIN,
            c2s_call => join,
            c2s_handler => {?MODULE, join, 2},
            s2c_call => join,
            encoder => demo_pb
        },
        #{
            opcode => ?ZONE_PART,
            c2s_call => part,
            c2s_handler => {?MODULE, part, 2},
            s2c_call => part,
            encoder => demo_pb
        },
        #{
            opcode => ?ZONE_STATE_XFER,
            s2c_call => state_transfer,
            encoder => demo_pb
        }
    ].

%%%====================================================================
%%% API
%%%====================================================================

-spec join(nonempty_binary(), saline_session:session()) ->
    {atom(), saline_session:session()}.
join(EncodedMsg, Session) ->
    % Ensure that this client is authenticated
    Auth = saline_session:is_authenticated(Session),
    join(EncodedMsg, Auth, Session).

-spec join(nonempty_binary(), boolean(), saline_session:session()) ->
    {atom(), saline_session:session()}.
join(EncodedMsg, _Auth = true, Session) ->
    % Get the player's unique session ID and the current game info
    ID = saline_session:get_id(Session),
    PID = saline_session:get_pid(Session),
    GameInfo = saline_session:get_game_info(Session),
    % Get the zone that the player wants to join and the player's handle.
    Message = ks_pb:decode_msg(EncodedMsg, join),
    Name = maps:get(name, Message),
    Zone = maps:get(zone, Message),
    % Set the session type to 'pc', for player character.
    SessionInfo = {pc, PID},
    % Join the zone and update the player's session accordingly, including an
    % automatic termination callback if/when the user closes their session
    % unexpectedly.
    SessionUpdate =
        case ks_zone2:join(Name, ID, SessionInfo, Zone) of
            ok ->
                S1 = saline_session:set_game_info(
                    GameInfo#{zone => Zone}, Session
                ),
                TermCall = {?MODULE, part, 1},
                saline_session:set_termination_callback(TermCall, S1);
            Error ->
                % Note the error and do nothing to the session
                logger:debug("Error joining zone: ~p", [Error]),
                Session
        end,
    {ok, SessionUpdate};
join(_EncodedMsg, _, Session) ->
    % Don't even bother decoding the message if the user isn't authenticated,
    logger:warning("Unauthenticated user trying to join: ~p", [Session]),
    {ok, Session}.

-spec part(saline_session:session()) -> ok.
part(Session) ->
    ID = saline_session:get_id(Session),
    PID = saline_session:get_pid(Session),
    GameInfo = saline_session:get_game_info(Session),
    Zone = maps:get(zone, GameInfo),
    SessionInfo = {pc, PID},
    ks_zone2:part(ID, SessionInfo, Zone),
    S1 = saline_session:set_game_info(GameInfo#{zone => undefined}, Session),
    S2 = saline_session:set_termination_callback(undefined, S1),
    {ok, S2}.

-spec action(nonempty_binary(), saline_session:session()) ->
    {atom(), saline_session:session()}.
action(EncodedMsg, Session) ->
    % Ensure that this client is authenticated
    Auth = saline_session:is_authenticated(Session),
    action(EncodedMsg, Auth, Session).

-spec action(nonempty_binary(), boolean(), saline_session:session()) ->
    {atom(), saline_session:session()}.
action(EncodedMsg, _Auth = true, Session) ->
    Msg = ks_pb:decode_msg(EncodedMsg, action),
    Latency = saline_session:get_latency(Session),
    ID = saline_session:get_id(Session),
    GameInfo = saline_session:get_game_info(Session),
    Zone = maps:get(zone, GameInfo),
    ks_zone2:action(Msg, Latency, ID, Zone);
action(_EncodedMsg, _, Session) ->
    % Don't even bother decoding the message if the user isn't authenticated,
    logger:warning("Unauthenticated user trying to send actions: ~p", [Session]),
    {ok, Session}.

-spec state_transfer(tuple(), [pid(), ...]) -> ok.
state_transfer(State, Who) ->
    {Sequence, Actions, Projectiles, Collisions, Entities} = State,
    OpCode = <<?ZONE_STATE_XFER:16>>,
    Msg = ks_pb:encode_msg(
        #{
            sequence => Sequence,
            actions => Actions,
            projectiles => Projectiles,
            collisions => Collisions,
            entities => Entities
        },
        state_transfer
    ),
    Send = fun(Pid) ->
        Pid ! {self(), broadcast, [OpCode, Msg]}
    end,
    lists:foreach(Send, Who).
