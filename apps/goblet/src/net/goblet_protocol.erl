-module(goblet_protocol).

%-export([
%    decode/2
%]).
%
%-include("net/goblet_session.hrl").
%-include("db/goblet_database.hrl").
%-include("goblet_pb.hrl").
%
%-include_lib("kernel/include/logger.hrl").
%-include_lib("eunit/include/eunit.hrl").
%
%%%=========================================================================
%%% Goblet Protocol.
%%%
%%% This module handles decoding serialized messages and routing them to the
%%% correct place for further processing and/or replies.
%%%
%%%=========================================================================
%
%%%-------------------------------------------------------------------------
%%% @doc Decode messages from clients and route them to an appropriate
%%%      function
%%% @end
%%%-------------------------------------------------------------------------
%-spec decode(binary(), session()) -> {ok, session()} | {msg(), session()}.
%decode(<<?VERSION:16, _T/binary>>, State) ->
%    logger:notice("Version request"),
%    {ok, State};
%decode(<<?HEARTBEAT:16, _T/binary>>, State) ->
%    logger:debug("Got heartbeat"),
%    {ok, State};
%decode(<<?ACCOUNT_NEW:16, T/binary>>, _State) ->
%    logger:notice("New account request"),
%    goblet_account:new(T);
%decode(<<?ACCOUNT_LOGIN:16, T/binary>>, _State) ->
%    logger:notice("Account login request"),
%    goblet_account:login(T);
%decode(<<?SHIP_VALIDATE:16, T/binary>>, State) ->
%    logger:notice("Ship validation packet from ~p", [State#session.email]),
%    goblet_game_msg:ship_validate(T, State);
%decode(<<OpCode:16, _T/binary>>, State) ->
%    logger:notice("Got an unknown opcode ~p from ~p", [
%        OpCode,
%        State#session.email
%    ]),
%    {ok, State}.
%
%%%=========================================================================
%%% Internal functions
%%%=========================================================================
