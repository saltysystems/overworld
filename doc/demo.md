# Building your first Saline application: Chat
**NOTE**: This documentation is unfinished! Use at your own risk.

## Getting started

### Get the latest Saline code

Clone the repository:

```bash
$ git clone https://github.com/saltysystems/saline
```

### Creating a new app

Saline is already set up to be a rebar3 umbrella application. You need only
create a new app, we will call it `chat`: 

```bash
$ cd saline/apps
$ rebar3 new app chat
```

### Building our chat app
While chat isn't particularly exciting, it lets us dive directly into
building a Saline application and highlights some of the features of the
framework.


### Hello, World!
Make a new Erlang source file called `chat_global.erl` in `apps/chat/src`.

You'll want to specify that this is a `gen_zone` server and export the minimum
required callback functions:
```erlang
-module(chat_global).
-behaviour(gen_zone).

% Required gen_zone callbacks
-export([
         init/1,
         handle_join/3,
         handle_part/2,
         handle_rpc/4,
         handle_tick/2
        ]).
```

Next let's start to define the API.

We'll need some way to start and stop the server (start/0, stop/0), a way for
players to join the session (join/2), a way for players to leave (part/1), and
a way for players to send some input to the server (send_msg/2). For each
function here, `gen_zone` will call back to our handler functions defined
above. This will allow us to expose a standard API to our clients and hide the
implementation details. Saline will seamlessly handle messages sent over the
network to remote clients (via Protobuf) or to other Erlang processes
(including distributed Erlang!).

Saline will automatically keep track of any sessions connected to our world
server and will handle message serialization/deserialization between the
clients and the server.

```erlang
-export([
         start/0,
         stop/0,
         join/2,
         part/1,
         send_msg/2
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

send_msg(Msg, Session) ->
    gen_zone:rpc(?SERVER, send_msg, Msg, Session).

```

Now that we have defined the API boilerplate, we can start writing some
application code!

The first thing we need to do is setup the initial gen_zone server state. By
default, gen_zone will let anyone connect without authentication and will
update the game world at 30 ticks per second. This should be totally OK for our
purposes. 

Normally for a game we'll want to define some initial state and mutate that
state over time as players interact with the game world in fun and exciting
ways. For a chat application, we'll use the server state to buffer all messages
received every tick, and then dump the contents of the buffer to all connected
players every tick. Let's just make the initial state an empty list for now.

```erlang
init([]) ->
    InitialState = [],
    {ok, InitialState}.
```

Next we'll need to define what happens when players join or leave the game.
Saline will automatically keep track of any sessions connected to our world
server and will handle message serialization/deserialization between the
clients and the server. So all you need to write is the game logic!

#### Aside: Saline messages
At the end of any required callback function, you'll want to return a 3-tuple
to `gen_zone` with the following rules:
```erlang
    {Status, Response, State} when
        Status :: ok | {ok, Session},
        Response :: noreply 
                    | {'@zone', term()}
                    | {'@', list(), term()},
        State :: term().
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
Now we should implement the handler functions for our chat server. We'll add
some logger business to give us some idea of what's going on, but otherwise do
nothing very exciting.

```erlang
handle_join(_Msg, Session, _Players, State) ->
    ID = saline_session:get_id(Session),
    logger:notice("Player ~p has joined the server!", [ID]),
    {ok, noreply, State}.

handle_part(Session, _Players, State) ->
    ID = saline_session:get_id(Session),
    logger:notice("Player ~p has left the server!", [ID]),
    {ok, noreply, State}.
```

For RPCs, we know we have one type function that a client might call:
`send_msg`. Let's say that `send_msg` will include, obviously, the message.
Let's also spice it up a bit and say that we'll include a hexadecimal color
code for our chat message. 

Notice that the function signature is a bit different for `handle_rpc` - we
must additionally include the message type. This will be especially important
when we start serializing the data over a WebSocket, because we'll need to
instruct Saline what Protobuf message to use.

We'll take the message verbatim and stuff it into the zone state. You may want
to modify the message before buffering it, see the extra exercises at the end
of the doc.

```erlang
handle_rpc(send_msg, Msg, Session, _Players, State) ->
    ID = saline_session:get_id(Session),
    logger:notice("Player ~p has sent a chat message: ~p", [ID, Msg]),
    {ok, noreply, State}.
```

There are a few more callbacks that we need to define to properly implement the
`gen_zone` behavior. Since the game server is going to update every 30ms, we
have to implement logic for that update. For now we'll do the minimum necessary
to get the server to compile:

```erlang
handle_tick(_Players, State) ->
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
===> Booted sasl
1> 
```

From the shell you can start the world server and verify everything is working:
```
1> chat_global:start().
{ok,<0.482.0>}
2>
3> chat_global:stop().
ok
```

Let's also try to join the server as a player from the shell. First we need to create a Saline session:
```
8> S = saline_session:new().
{session,-576460752303423295,undefined,none,false,0,
         undefined,undefined}
```

We can then join and part the server with this session information. Remember
that join/2 requires a message, so let's assume that's the handle of the player
joining. When we work with protobuf later, we'll see that we mostly deal with
messages in terms of maps. So let's sketch out what that message might look
like:

```
8> Name = #{ name => "rambo" },
9> chat_global:join(Name, S).
2022-06-22T21:21:11.872998-05:00 : notice: Player -576460752303423295 has joined the server!
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
10> chat_global:part(S).
2022-06-22T21:21:21.243792-05:00 : notice: Player -576460752303423295 has left the server!
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
```


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


# Exercises left to the reader
Consider changing the chat buffer to be a circular buffer. Hold a certain
volume of messages and trim either based on timestamp (e.g., all messages in
the last 5 minutes) or volume (e.g., the last 100 messages). What are the
tradeoffs?

Try changing the `handle_rpc` function to modify a chat message on the fly.
Maybe you want to implement a bad word filter?
