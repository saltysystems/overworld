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
-export([join/3, part/2, action/3]).

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
%%%
join(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({join, Msg, Session})).

part(ServerRef, Session) ->
    gen_server:call(ServerRef, ?TAG_I({part, Session})).

action(ServerRef, Msg, Session) ->
    gen_server:call(ServerRef, ?TAG_I({action, Msg, Session})).

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
                tick_rate = ?TICK_RATE
            }};
        ignore ->
            ignore;
        Stop = {stop, _Reason} ->
            Stop
    end.

handle_call(?TAG_I({Fun, Msg, Session}), _From, St0 = #state{cb_mod = CbMod}) ->
    Reply =
        case gremlin_session:is_authenticated(Session) of
            true ->
                case Fun of
                    part ->
                        CbMod:part(Session);
                    join ->
                        CbMod:join(Msg, Session);
                    action ->
                        CbMod:action(Msg, Session)
                end;
            _ ->
                % not authorized
                {ok, Session}
        end,
    {reply, Reply, St0};
handle_call(_Call, _From, St0) ->
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
