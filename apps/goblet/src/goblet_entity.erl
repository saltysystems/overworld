-module(goblet_entity).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_statem).

%% Public API
-export([start/2, stop/0, decide_next_move/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).

%% State callbacks, don't call directly
-export([idle/3, flee/3]).

-define(ENTITY(UniqueID), {via, gproc, {n,l,{?MODULE, UniqueID}}}).

%-record(entity {
%	id :: pos_integer(),
%	health :: integer(),
%	energy :: integer(),
%	inventory :: list()
%}).

%% API
-spec decide_next_move(pos_integer(),list()) -> list().
decide_next_move(ID, Board) ->
	gen_statem:call(?ENTITY(ID), decide).

% Mandatory callbacks
terminate(_Reason, _State_, _Data) ->
	void.
code_change(_Vsn, State, Data, _Extra) ->
	{ok, State, Data}.
init({Name, UniqueID}) ->
	% Spawn the named entity from the DB
	implementme.
callback_mode() -> state_functions.

start(Foo,Bar) -> 
	implementme.
stop() ->
	implementme.
	
idle({call, From}, decide, Data) ->
	implementme.
flee({call, From}, decide, Data) ->
	implementme.
