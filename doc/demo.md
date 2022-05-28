# Building your first game

## Getting started

### Get the latest Saline code

Clone the repository:

```
$ git clone https://github.com/saltysystems/saline
```

### Creating a new app

Saline is already set up to be a rebar3 umbrella application. You need only
create a new app, we will call it `chi`: 

```
$ cd saline/apps
$ rebar3 new app chi
```

### Making some decisions
At this point we need to decide what kind of game we're going to make. If we
want to make a game based on rounds or turns, we'll need to use `gen_match`. If
we want to make a game based on a constant tick timer, we'll use `gen_zone`.

For this project, we'll be putting together a "clicker" style game that let's
players compete for control over the territories of CHI TOWN. We want everyone
to be competing on the same field as the clock ticks by, so let's use
`gen_zone`. 

We'll start simply and call our first area the "world", which will have a
single timer that governs the tick rate for our players and keeps track of
their resources etc.

### Hello, World Server!
Make a new Erlang source file called `chi_worldserver.erl` in `apps/chi/src`.

You'll want to specify that this is a `gen_zone` server and export the required
callback functions:
```erlang
-module(chi_worldserver).
-behaviour(gen_zone).

% Required gen_zone callbacks
-export([
         init/1,
         handle_join/3,
         handle_part/3,
         handle_action/3,
         handle_tick/2
        ]).
```

Next let's start to define the API.

We'll need some way to start and stop the world server (start/0, stop/0), a way
for players to join the session (join/2), a way for players to leave (part/1),
and a way for players to send some input to the server (action/2).

Saline will automatically keep track of any sessions connected to our world
server and will handle message serialization/deserialization between the
clients and the server.

In the same file, add:
```
-export([
         start/0,
         stop/0,
         join/2,
         part/1,
         action/2
        ]).

-define(SERVER, ?MODULE).

% API
start() ->
    gen_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_zone:stop(?SERVER).

join(Msg, Session) ->
    gen_zone:join(?SERVER, Msg, Session).

part(Session) ->
    gen_zone:part(?SERVER, Session).

action(Msg, Session) ->
    gen_zone:action(?SERVER, Msg, Session).

```

Now that we have defined the API boilerplate, we can start writing some game code!

First thing's first, we need to define the initial world state. While we're
sort of figuring things out, we can play fast and loose and just use a map to
hold all of our game data. We'll set the game clock to tick once every 100
miliseconds, giving us 10 game updates per second.

```
init([]) ->
    InitialState = #{},
    TickRate = 100, % a leisurely 10 ticks/sec
    {ok, InitialState, TickRate}.
```

Next we'll need to define what happens when players join or leave the game.
Saline will automatically keep track of any sessions connected to our world
server and will handle message serialization/deserialization between the
clients and the server. So all you need to write is the game logic!

#### Aside: Saline messages
When completing a callback function, you'll want to return a 3-tuple to
`gen_zone` with the following rules:
```
    {Status, Response, State} when
        Status :: ok | {ok, Session},
        Response :: noreply 
                    | {'@zone', term()}
                    | {'@', list(), term()},
        State :: term()
```

In plain language: for `Status` messages, you want to either respond `ok` or
respond `ok` with an updated copy of the player's session. You may update the
player's session if there's something about it that your callback function
changes. Perhaps you wanted to set some game specific information in via
`saline_session:set_game_info/1`, for example.

For responses, `gen_zone` understands 3 different response messages from a
server implementing the behaviour. You can use `'@zone'` to broadcast a message
to everyone in the zone, in our case everyone connected to the World Server.
You can also target individuals with `{'@', PlayerIDs}` where PlayerIDs is a
list of player IDs that should receive this message. Lastly, you can use
`noreply` to silently accept the message with no updates sent out to connected
players. Here's a handy reference table:

|  Response       | Description |
| --------------- | ----------- |
| `{'@zone', Msg}` | Send a zone-wide message *Msg* to all connected clients |
| `{'@', PlayerID, Msg}` | Send a message, *Msg*, to player *PlayerID*. Also accepts a list of Player IDs |
| `noreply` | Send no reply to any connected player |

Finally, you should return your internal state to `gen_zone` via `State`.

### Implementing the callbacks

Since we don't know exactly what the game state is going to look like just yet,
let's stub these out with some simple logger notices.

```
handle_join(_Msg, Session, State) ->
    PID = saline_session:get_pid(Session),
    logger:notice("Process ~p has joined the game!", [PID]),
    {ok, noreply, State}.

handle_part(Session, State) ->
    PID = saline_session:get_pid(Session),
    logger:notice("Process ~p has left the game!", [PID]),
    {ok, noreply, State}.
```

Likewise, actions will have no effect right now but we'll add a log message
there, as well:
```
handle_action(Msg, Session, State) ->
    PID = saline_session:get_pid(Session),
    logger:notice("Process ~p has sent an action: ~p", [PID, Msg]),
    {ok, noreply, State}.
```

There are a few more callbacks that we need to define to properly implement the
`gen_zone` behavior. Since the game server is going to update every 100ms, we
have to implement logic for that update. This is done via the `handle_tick` function:

```
handle_tick(_Players, State) ->
    logger:notice("Got a tick!"),
    {ok, noreply, State}.
```

### Testing it out so far 
You can save up your work at this point and try things out.

```
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling saline
===> Compiling chi
Erlang/OTP 25 [erts-13.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns] [dtrace] [sharing-preserving]

Eshell V13.0  (abort with ^G)
===> Booted gproc
===> Booted mnesia
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted saline
===> Booted ks
===> Booted sasl
1> 
```

From the shell you can start the world server and verify everything is working:
```
1> chi_worldserver:start().
{ok,<0.482.0>}
2>
2> 2022-05-28T11:53:45.405605-05:00 : notice: Got a tick!
2022-05-28T11:53:45.509662-05:00 : notice: Got a tick!
2022-05-28T11:53:45.610799-05:00 : notice: Got a tick!
2022-05-28T11:53:45.711791-05:00 : notice: Got a tick!
...
3> chi_worldserver:stop().
ok
```

Now that we know the world server is accepting ticks, we'll remove the notice
since it's a bit of a nuisance. Change the following:
```
    logger:notice("Got a tick!")
```
to
```
    logger:debug("Got a tick!")
```

That way we'll still have the message should we need to turn up the logger
verbosity later.

If you still have your Erlang shell open, you can simply run
```
5> c(chi_worldserver).
```
To recompile the world server with the latest version of the code. Now when you
start the server, the shell will be mercifully terse:
```
7> chi_worldserver:start().
{ok,<0.492.0>}
8>
```

Let's also try to join the server as a player from the shell. First we need to create a Saline session:
```
8> S = saline_session:new().
{session,-576460752303423295,undefined,none,false,0,
         undefined,undefined}
```

We can then join and part the server with this session information:

```
9> chi_worldserver:join("hello world!", S).
2022-05-28T14:23:52.409962-05:00 : notice: Player -576460752303423295 has joined the game!
{ok,{session,,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
10> chi_worldserver:part(S).
2022-05-28T14:25:09.754252-05:00 : notice: Player -576460752303423295 has left the game!
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
```


## Writing game logic

*TODO*

## Wiring up the client

### Serializing messages
If we ever want players to connect from the outside world, we'll need to define
some messages that we can serialize and send across the wire. Saline supports
[protobuf](https://developers.google.com/protocol-buffers) as a standard wire format
to communicate between game server and client.

### Saline and Godot
If you plan to write your client in [Godot](https://godotengine.org/), you can
add the Saline plugin to your Godot game and automatically generate a client
library based on your protobuf schema.
[https://github.com/saltysystems/saline_client](https://github.com/saltysystems/saline_client)

