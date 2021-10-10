-module(goblet_game_msg).

-behaviour(goblet_rpc).

% Required callbacks for Goblet
-export([rpc_info/0]).

-export([
    ship_validate/2
]).

-include("shipgame_pb.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------
%% @doc Register the ?SHIP_VALIDATE opcode and associated function
%% @end
%%------------------------------------------------------------------------
-define(SHIP_VALIDATE, 16#1300).

-spec rpc_info() -> [{pos_integer(), mfa()}, ...].
rpc_info() ->
    [
        {?SHIP_VALIDATE, {?MODULE, ship_validate, 2}}
    ].

%%------------------------------------------------------------------------
%% @doc Validate a ship design
%% @end
%%------------------------------------------------------------------------
-spec ship_validate(binary(), goblet_session:session()) ->
    {[binary(), ...], goblet_session:session()}.
ship_validate(Message, Session) ->
    % Just crash if this is not true.
    Authenticated = goblet_session:get_authenticated(Session),
    ship_validate(Message, Authenticated, Session).

-spec ship_validate(binary(), boolean(), goblet_session:session()) ->
    {[binary(), ...], goblet_session:session()}.
ship_validate(Message, true, Session) ->
    Decode = shipgame_pb:decode_msg(Message, 'ShipValidateReq'),
    Components = Decode#'ShipValidateReq'.components,
    Resp =
        case validate_grid(Components, goblet_shipgrid:new()) of
            true ->
                #'ResponseObject'{status = 'OK'};
            false ->
                #'ResponseObject'{
                    status = 'ERROR',
                    error = "Invalid configuration"
                }
        end,
    Msg = shipgame_pb:encode_msg(#'ShipValidateResp'{resp = Resp}),
    OpCode = <<?SHIP_VALIDATE:16>>,
    {[OpCode, Msg], Session}.

-spec validate_grid(list(), map()) -> boolean().
validate_grid([], Map) ->
    goblet_shipgrid:validate(Map);
validate_grid([C | Rest], ShipGrid0) ->
    {X, Y} =
        {C#'ShipValidateReq.Component'.x, C#'ShipValidateReq.Component'.y},
    ID = C#'ShipValidateReq.Component'.id,
    Rotation = C#'ShipValidateReq.Component'.rotation,
    Object = goblet_ship:get(ID),
    ShipGrid1 = goblet_shipgrid:put_cell(
        {X, Y},
        Object,
        Rotation,
        ShipGrid0
    ),
    validate_grid(Rest, ShipGrid1).
