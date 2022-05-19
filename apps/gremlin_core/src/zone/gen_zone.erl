-module(gen_zone).
-behaviour(gen_server).

% gen_server
-export([start/3, start/4]).
-export([start_link/3, start_link/4]).
-export([start_monitor/3, start_monitor/4]).
-export([call/2, call/3]).
-export([cast/2]).
-export([reply/2]).
-export([stop/1, stop/3]).
% gen_zone specific calls

% must have corresponding callbacks
-export([join/3, part/2, action/3]).
% handled internally
-export([who/1]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(TAG_I(Msg), {'$gen_zone_internal', Msg}).

-record(state, {
    cb_mod :: module(),
    cb_data :: term(),
    players :: map(),
    tick_timer :: undefined | reference(),
    tick_rate :: pos_integer()
}).

-define(TICK_RATE, 30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_zone callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-callback init(Args) -> Result when
    Args :: term(),
    Result ::
        {ok, InitialData}
        | ignore
        | {stop, Reason},
    InitialData :: term(),
    Reason :: term().

-callback handle_join(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: gremlin_session:session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_part(Session, State) -> Result when
    Session :: gremlin_session:session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_action(Msg, Session, State) -> Result when
    Msg :: term(),
    Session :: gremlin_session:session(),
    State :: term(),
    Result :: {ok, Session}.

-callback handle_tick(State) -> Result when
    State :: term(),
    Result :: term().

-callback handle_state_xfer(State) -> Result when
    State :: term(),
    Result :: term().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Module, Args, Opts) ->
    gen_server:start(?MODULE, {Module, Args}, Opts).

start(ServerName, Module, Args, Opts) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Opts).

start_link(Module, Args, Opts) ->
    gen_server:start_link(?MODULE, {Module, Args}, Opts).

start_link(ServerName, Module, Args, Opts) ->
    % TODO: Check for configuration options in Opts
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Opts).

start_monitor(Module, Args, Opts) ->
    gen_server:start_monitor(?MODULE, {Module, Args}, Opts).

start_monitor(ServerName, Module, Args, Opts) ->
    gen_server:start_monitor(ServerName, ?MODULE, {Module, Args}, Opts).

call(ServerRef, Msg) ->
    gen_server:call(ServerRef, Msg).

call(ServerRef, Msg, Timeout) ->
    gen_server:call(ServerRef, Msg, Timeout).

cast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, Msg).

reply(From, Reply) ->
    gen_server:reply(From, Reply).

stop(ServerRef) ->
    gen_server:stop(ServerRef).

stop(ServerRef, Reason, Timeout) ->
    gen_server:stop(ServerRef, Reason, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% public behavior API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(ServerRef, Msg, Session) ->
    % Check that the session is valid before doing anything
    case gremlin_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({join, Msg, Session}));
        false ->
            {ok, Session}
    end.

part(ServerRef, Session) ->
    % Check that the session is valid before doing anything
    case gremlin_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({part, Session}));
        false ->
            {ok, Session}
    end.

action(ServerRef, Msg, Session) ->
    case gremlin_session:is_authenticated(Session) of
        true ->
            gen_server:call(ServerRef, ?TAG_I({action, Msg, Session}));
        false ->
            {ok, Session}
    end.

% Return list of players
who(ServerRef) ->
    gen_server:call(ServerRef, ?TAG_I(who)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback functions for internal state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Initialize the internal state of the zone, with timer
init({CbMod, CbArgs}) ->
    case CbMod:init(CbArgs) of
        {ok, CbData} ->
            Timer = erlang:send_after(1, self(), ?TAG_I(tick)),
            {ok, #state{
                cb_mod = CbMod,
                cb_data = CbData,
                tick_timer = Timer,
                tick_rate = ?TICK_RATE,
                players = #{}
            }};
        ignore ->
            ignore;
        Stop = {stop, _Reason} ->
            Stop
    end.

handle_call(?TAG_I({join, Msg, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    % This can fail, so check for success.
    {Reply, St1} =
        case CbMod:handle_join(Msg, Session, CbData) of
            {ok, Resp} ->
                % Create a new key for this player ID, containing the pid and
                % type
                Players0 = St0#state.players,
                {ID, PID, Type} = player_info(Session),
                Players1 = Players0#{ID => {PID, Type}},
                St = St0#state{players = Players1},
                {{ok, Resp}, St};
            Other ->
                {Other, St0}
        end,
    {reply, Reply, St1};
handle_call(?TAG_I({part, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    % Delete the key identified by the Session ID
    ID = gremlin_session:get_id(Session),
    P1 = maps:remove(ID, St0#state.players),
    St1 = St0#state{players = P1},
    Reply = CbMod:handle_part(Session, CbData),
    {reply, Reply, St1};
handle_call(?TAG_I({action, Msg, Session}), _From, St0) ->
    CbMod = St0#state.cb_mod,
    CbData = St0#state.cb_data,
    Reply = CbMod:handle_action(Msg, Session, CbData),
    {reply, Reply, St0};
handle_call(?TAG_I(who), _From, St0) ->
    Players = St0#state.players,
    Who = maps:keys(Players),
    {reply, Who, St0};
handle_call(Call, _From, St0) ->
    io:format("Got a not-well-understood call: ~p~n", [Call]),
    {reply, ok, St0}.

handle_cast(_Cast, St0) ->
    {noreply, St0}.

handle_info(?TAG_I(tick), St0) ->
    TimerRef = St0#state.tick_timer,
    erlang:cancel_timer(TimerRef),
    TickRate = St0#state.tick_rate,
    T1 = erlang:send_after(TickRate, self(), ?TAG_I(tick)),
    St1 = tick(St0),
    {noreply, St1#state{tick_timer = T1}}.

terminate(_Reason, _St0) -> ok.
code_change(_OldVsn, St0, _Extra) -> {ok, St0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tick(St0 = #state{cb_mod = CbMod, cb_data = CbData0}) ->
    case CbMod:handle_tick(CbData0) of
        {ok, CbData1} ->
            St0#state{cb_data = CbData1}
    end.

player_info(Session) ->
    ID = gremlin_session:get_id(Session),
    PID = gremlin_session:get_pid(Session),
    Type = gremlin_session:get_type(Session),
    {ID, PID, Type}.
