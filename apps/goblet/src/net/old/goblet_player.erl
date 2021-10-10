-module(goblet_player).

%-export([
%        new/2, 
%		list/2,
%		state_update/2
%        ]).
%
%-include("net/goblet_session.hrl").
%-include("db/goblet_database.hrl").
%-include("shipgame_pb.hrl").
%
%-include_lib("kernel/include/logger.hrl").
%-include_lib("eunit/include/eunit.hrl").
%
%-define(PLAYER_NEW, 16#1050).
%-define(PLAYER_INFO, 16#1060).
%-define(PLAYER_LIST, 16#1055).
%
%-record(session, {authenticated, email}).
%
%%%------------------------------------------------------------------------
%%% @doc Create a new player character
%%% @end
%%%------------------------------------------------------------------------
%-spec new(binary(), tuple()) -> {[binary(), ...], tuple()}.
%% Let it crash when an unauthenticated user tries to make an account.
%new(Message, State) when State#session.authenticated =:= true ->
%    Decode = shipgame_pb:decode_msg(Message, 'PlayerNewReq'),
%    Name = Decode#'PlayerNewReq'.name,
%    Symbols = Decode#'PlayerNewReq'.symbol,
%    Colors = Decode#'PlayerNewReq'.color,
%    Role = Decode#'PlayerNewReq'.role,
%    Account = State#session.email,
%    Msg =
%        case goblet_game:new_player(Name, Colors, Symbols, Role, Account) of
%            ok ->
%                shipgame_pb:encode_msg(#'PlayerNewResp'{status = 'OK'});
%            {error, Error} ->
%                shipgame_pb:encode_msg(#'PlayerNewResp'{
%                    status = 'ERROR',
%                    error = Error
%                })
%        end,
%    OpCode = <<?PLAYER_NEW:16>>,
%    {[OpCode, Msg], State}.
%
%new_test() ->
%    % mock the state
%    Email = "TestUser@doesntexist.notadomain",
%    State = #session{email = Email, authenticated = true},
%    Message = shipgame_pb:encode_msg(#'PlayerNewReq'{
%        name = "Chester The Tester",
%        color = ["#000000", "#ffffff", "#c0ffee"],
%        symbol = [1, 3],
%        role = 'INTERCEPTOR'
%    }),
%    {[RespOp, RespMsg], _State} = new(
%        Message,
%        State
%    ),
%    ?assertEqual(RespOp, <<?PLAYER_NEW:16>>),
%    RespObj = shipgame_pb:decode_msg(RespMsg, 'PlayerNewResp'),
%    ?assertEqual(RespObj#'PlayerNewResp'.status, 'OK').
%
%%%------------------------------------------------------------------------
%%% @doc List all player characters for an account
%%% @end
%%%------------------------------------------------------------------------
%-spec list(binary(), tuple()) -> {[binary(), ...], tuple()}.
%list(_Message, State) when State#session.authenticated =:= true ->
%    logger:notice("Session is authenticated, continuing.."),
%    Email = State#session.email,
%    Msg =
%        case goblet_db:account_by_email(Email) of
%            {error, Err} ->
%                shipgame_pb:encode_msg(#'PlayerListResp'{
%                    status = 'ERROR',
%                    error = Err
%                });
%            Record ->
%                shipgame_pb:encode_msg(#'PlayerListResp'{
%                    status = 'OK',
%                    players = Record#goblet_account.player_ids
%                })
%        end,
%    OpCode = <<?PLAYER_LIST:16>>,
%    {[OpCode, Msg], State};
%list(_Message, State) ->
%    goblet_protocol:default_handler(State).
%
%list_test() ->
%    Email = "TestUser@doesntexist.notadomain",
%    State = #session{email = Email, authenticated = true},
%    {[RespOp, RespMsg], _State} = list(<<>>, State),
%    ?assertEqual(RespOp, <<?PLAYER_LIST:16>>),
%    RespObj = shipgame_pb:decode_msg(RespMsg, 'PlayerListResp'),
%    PlayerList = RespObj#'PlayerListResp'.players,
%    ?assertEqual(is_list(PlayerList), true).
%
%%%------------------------------------------------------------------------
%%% @doc Update a player's info
%%% @end
%%%------------------------------------------------------------------------
%-spec state_update(
%    {
%        list(),
%        integer(),
%        integer(),
%        list(),
%        list()
%    },
%    pos_integer()
%) -> ok.
%state_update({Name, Health, Energy, Flags, Inventory}, MatchID) ->
%    Update = #'PlayerInfoResp'{
%        name = Name,
%        health = Health,
%        energy = Energy,
%        flags = Flags,
%        inventory = Inventory
%    },
%    Msg = shipgame_pb:encode_msg(Update),
%    OpCode = <<?PLAYER_INFO:16>>,
%    notify([OpCode, Msg], MatchID, Name).
%
% 
%% internal
%
%-spec notify([binary(), ...], pos_integer(), list()) -> ok.
%notify(Message, MatchID, Player) ->
%    logger:debug("Sending a notification to ~p", [Player]),
%    gproc:send({p, l, {match, MatchID}}, {self(), notify, Message, Player}).
%
%
