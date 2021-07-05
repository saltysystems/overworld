-module(goblet_entity).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([start/0, stop/0]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).


