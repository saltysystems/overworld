-module(ow_player_reg).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

% Public interface
-export([
    start_link/0,
    stop/0,
    new/5,
    new/4,
    delete/1,
    get/1,
    update/1,
    list/1
]).
% Helpers for the player() type
-export([
    get_id/1,
    get_pid/1,
    set_pid/2,
    get_serializer/1,
    set_serializer/2,
    get_zone/1,
    set_zone/2,
    get_info/1,
    set_info/2
]).
% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% internal state
-record(player, {
    id :: integer(),
    pid :: pid() | undefined,
    serializer :: ow_session:serializer(),
    zone :: pid() | atom(),
    info :: term()
}).
-type player() :: #player{}.
-export_type([player/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    gen_server:stop({local, ?SERVER}).

-spec new(integer(), pid(), ow_session:serializer(), pid()) ->
    ok | {error, atom()}.
new(SID, PID, Serializer, Zone) ->
    new(SID, PID, Serializer, Zone, undefined).
-spec new(integer(), pid(), ow_session:serializer(), pid(), term()) ->
    ok | {error, atom()}.
new(SID, PID, Serializer, Zone, Info) ->
    Player = #player{
        id = SID,
        pid = PID,
        serializer = Serializer,
        zone = Zone,
        info = Info
    },
    gen_server:call(?SERVER, {new, Player}).

-spec get(integer()) -> player() | {error, atom()}.
get(SID) ->
    gen_server:call(?SERVER, {get, SID}).

-spec update(player()) -> ok | {error, atom()}.
update(Player) ->
    gen_server:call(?SERVER, {update, Player}).

-spec delete(integer()) -> ok.
delete(SID) ->
    gen_server:call(?SERVER, {delete, SID}).

-spec list(pid()) -> [integer(), ...].
list(ZonePID) ->
    gen_server:call(?SERVER, {list, ZonePID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Functions                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_id(player()) -> integer().
get_id(Player) -> Player#player.id.

-spec get_pid(player()) -> pid().
get_pid(Player) -> Player#player.pid.

-spec set_pid(pid(), player()) -> player().
set_pid(PID, Player) -> Player#player{pid = PID}.

-spec get_serializer(player()) -> ow_session:serializer().
get_serializer(Player) -> Player#player.serializer.

-spec set_serializer(ow_session:serializer(), player()) -> player().
set_serializer(Serializer, Player) ->
    Player#player{serializer = Serializer}.

-spec get_zone(player()) -> pid() | atom().
get_zone(Player) -> Player#player.zone.

-spec set_zone(pid() | atom(), player()) -> player().
set_zone(Zone, Player) -> Player#player{zone = Zone}.

-spec get_info(player()) -> term().
get_info(Player) -> Player#player.info.

-spec set_info(term(), player()) -> player().
set_info(Info, Player) -> Player#player{info = Info}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callbacks                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    TableName = global_player_registry,
    Table = ets:new(TableName, [{keypos, 2}]),
    {ok, Table}.

handle_call({new, Player}, _From, Table) ->
    Reply =
        case ets:lookup(Table, Player#player.id) of
            [] ->
                % Player doesn't exist, OK to add.
                ets:insert(Table, Player),
                ok;
            _ ->
                {error, already_added}
        end,
    {reply, Reply, Table};
handle_call({get, SID}, _From, Table) ->
    Reply =
        case ets:lookup(Table, SID) of
            [] ->
                {error, no_such_player};
            [Player] ->
                Player
        end,
    {reply, Reply, Table};
handle_call({update, Player}, _From, Table) ->
    Reply =
        case ets:lookup(Table, Player#player.id) of
            [] ->
                % Player doesn't exist, not sure if safe to add.
                {error, no_such_player};
            _ ->
                ets:insert(Table, Player)
        end,
    {reply, Reply, Table};
handle_call({delete, SID}, _From, Table) ->
    Reply =
        case ets:lookup(Table, SID) of
            [] ->
                ok;
            _Record ->
                ets:delete(Table, SID),
                ok
        end,
    {reply, Reply, Table};
handle_call({list, ZonePID}, _From, Table) ->
    Matches = ets:match_object(Table, #player{zone = ZonePID, _ = '_'}),
    {reply, Matches, Table}.

handle_cast(_Cast, Table) ->
    {noreply, Table}.

handle_info(_Info, Table) ->
    {noreply, Table}.

terminate(_Reason, _Table) ->
    ok.

code_change(_OldVsn, Table, _Extra) ->
    {ok, Table}.
