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
a way for players to send some input to the server (send/2). For each
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
         send/2
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

send(Msg, Session) ->
    gen_zone:rpc(?SERVER, chat_msg, Msg, Session).

```

You may have noticed that for the send/2 function, we send along the atom
`chat_msg` - this atom will ultimately refer to the name of one of the messages
we'll define in our wire protocol. 

Now that we have defined the API boilerplate, we can start writing some
application code! The first thing we need to do is setup the initial gen_zone
server state. By default, gen_zone will let anyone connect without
authentication and will update the game world at 30 ticks per second. This
should be totally OK for our purposes. 

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
`chat_msg`. Let's say that `chat_msg` will include, obviously, the message.
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
handle_rpc(chat_msg, Msg, Session, _Players, State) ->
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

### Saving state and sending messages
Now that we've confirmed that the server is up and running, and we're logging
some semi-useful messages, we actually need to save and buffer those messages
and send them to connected players. The first thing that we'll need to do is
add messages to the state. Let's modify the `handle_rpc` function to do that:
```
handle_rpc(chat_msg, Msg, Session, Players, State) ->
    ID = saline_session:get_id(Session),
    % Make sure the player has actually joined the zone
    State1 = 
        case gen_zone:is_player(ID, Players) of 
            false ->
                State;
            true ->
                [ #{ who => ID, msg => Msg } | State ]
        end,
    {ok, noreply, State1}.
```

A few things going on here. One, we check if the player sending the message is
actually in the zone. If so, we append the sender's ID and the message to the
zone state. 

#### Aside - Maps

You may be wondering why we're using maps to store all the data. The reason is
pretty simple - if we ever want send data over the wire via Protobuf, using a
map is the most flexible way to serialize it.

Now we'll need to have the zone send all of the messages it has buffered up in
each tick. To do so, we'll change the `handle_tick` function. First, we might
as well not send any updates to players when there are no messages to send. So
let's first head that off with a pattern match for empty state where we don't
reply anything, then follow it up with a zone-wide broadcast when we have
something to send.


```
handle_tick(_Players, []) ->
    {ok, noreply, []};
handle_tick(_Players, State) ->
    % Replace the current state with an empty list and send out the state.
    {state_transfer, {'@zone', State}, []}.
```

Let's try it out in the shell. We've got a number of things to do here if we have a fresh shell:
  1. Start a new session
  2. Set the PID of the session
  3. Start the chat server
  4. Join the server with our shell
  5. Send a message
  6. Finally, see if we get a response back.

So we'll proceed exactly along those lines:
```
1> S = saline_session:new().
2> S1 = saline_session:set_pid(self(), S).
{session,-576460752303423390,<0.538.0>,undefined,false,0,
         undefined,undefine}
3> chat_global:start().
{ok, <0.541.0>}
4> chat_global:join("soandso", S1).
2022-06-25T09:42:03.467787-05:00 : notice: Player -576460752303423390 has joined the server!
{ok,{session,-576460752303423390,<0.538.0>,undefined,false,
             0,undefined,undefined}}
5> chat_global:send(#{text => "hello world!", color => "red"}, S1).
2022-06-25T09:43:43.451675-05:00 : notice: Player -576460752303423390 has sent a chat message: #{color => "red",text => "hello world!"}
{ok,{session,-576460752303423390,<0.538.0>,undefined,false,
             0,undefined,undefined}}
6> flush().
Shell got {<0.541.0>,zone_msg,
           {state_transfer,[#{msg => #{color => "red",text => "hello world!"},
                              who => -576460752303423390}]}}
ok
```

### Serializing messages
If we ever want players to connect from the outside world, we'll need to define
some messages that we can serialize and send across the wire. Saline supports
[protobuf](https://developers.google.com/protocol-buffers) as a standard wire
format to communicate between game server and client.

Go ahead and make a `priv/proto` directory in the root of your chat
application, e.g., 
```bash
mkdir -p saline/apps/chat/priv/proto
```

Fire up your favorite editor and we'll start defining the protobuf file. I'll call it `chat.proto`.

```
syntax = "proto2"; 

package chat;
```

It's important that the package name matches the name of the application, for
our purposes. We can start defining some messages - we know for sure we need a
"join" message because that's a required callback for Saline. Probably all we
care about in the join message is the player's handle:

```
message join {
    required string handle = 1;
}
```

The next two messages have to do with receiving chats and sending them out to
clients in batches. The first one should be pretty self-explanatory: we want to
send along the player's chat message along with any color they'd like to send,
if they choose. The `state_transfer` message will simply tell the client to
expect a series of `chat_messages` as a list, since our `handle_tick/2`
function sends them out in batches.

```
message chat_msg {
  required string text = 1;
  optional bytes color = 2;
}

message state_transfer {
  repeated chat_msg msgs = 1;
}
```

That's it for the Protobuf file!

We have one last thing to do before Saline will automatically encode/decode
messages for us. We need to define a callback in our server, `rpc_info/0` that
will tell Saline some important information about our messages. Let's define that now:

```
% Saline RPCs
-define(CHAT_JOIN, 16#1001).
-define(CHAT_PART, 16#1002).
-define(CHAT_SEND, 16#1003).
-define(CHAT_XFER, 16#1004).

rpc_info() ->
    [
        #{
            opcode => ?CHAT_JOIN,
            c2s_handler => {?MODULE, join, 2},
            encode => chat_pb
        },
        #{
            opcode => ?CHAT_PART,
            c2s_handler => {?MODULE, part, 1},
            encode => chat_pb
        },
        #{
            opcode => ?CHAT_SEND,
            c2s_handler => {?MODULE, send, 2},
            encode => chat_pb
        },
        #{
            opcode => ?CHAT_XFER,
            s2c_call => state_transfer,
            encode => chat_pb
        }
    ].
```

So there's a few things going on here. First, we define some opcodes which are
a couple of bytes at the beginning of the message that let's the client and
saline determine which function to use to decode or encode the message. 

For messages that are received by the server, we need to define a
client-to-server handler, or `c2s_handler`. This should be a 3-tuple with the
module, function, and arity (MFA) that Saline will call whenever it receives a
message prefixed by the appropriate opcode. Since `join`, `part,` and `send`
are messages that come from the client, we inform Saline that they will have
corresponding `c2s_handler` functions when messages come in.

For the remaining message, we don't expect clients to send `state_transfer`
messages to our server since it represents a bundle of messages to be
dispatched all at once. So here we don't have a `c2s_handler`, but instead
define a server-to-client call, or `s2c_call`. The corresponding function for
our client will be created automatically by the Godot plugin.

Last but not least, make sure to add `rpc_info/0` to your exports! I added it
right after `handle_tick/2`:

```
-export([
         init/1,
         handle_join/4,
         handle_part/3,
         handle_rpc/5,
         handle_tick/2,
         rpc_info/0
        ]).
```

### Some finishing touches
Our chat server is in pretty good shape, but let's add a few finishing touches
to make it easier once we get to wiring up the client. The chat server really
ought to start automatically whenever you fire up the BEAM, and it will also
need to call into Saline to register its opcodes and associated callback
functions.

Open up `chat_app.erl` and make it look something like this:

```
-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    saline_protocol:register_app(chat),
    chat_sup:start_link().

stop(_State) ->
    ok.
```

We also need to make sure that the supervisor starts up our Chat server. You'll
need to edit `chat_sup.erl` and add a child spec for the chat server as such:
```

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                    #{
                        id => "chat_global",
                        start => {chat_global, start, []}
                     }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
```

We'll also want to edit the `rebar.conf` in the top-level directory, modifying
the `relx` tuple to include `chat` in the application startup list, e.g.:
```
{relx, [{release, {saline, "1.0.0"},
         [saline,
          sasl,
          mnesia,
          chat]},
...
```

You will also want to make sure to edit the `rebar.config` for your application
(i.e., `saline/apps/chat/rebar.config`) to set up GPB to encode/decode messages
properly:

```
{erl_opts, [
        debug_info,
        {i, "./_build/default/plugins/gpb/include"}
        ]}.

{gpb_opts, [
  {i, "priv/proto"},                % path/to/proto_dir
  {module_name_suffix, "_pb"}, % Naming convention, unless you have a good reason just go with it.
  {o_erl, "src"},              % Where the generated source is placed
  {o_hrl, "src"},          % Where the generated include files are placed
  {strings_as_binaries, false},
  {maps, true},
  {msgs_as_maps, true},
  {mapfields_as_maps, true},
  type_specs]}.


{provider_hooks, [
    {pre, [
        {compile, {protobuf, compile}},
        {clean, {protobuf, clean}}
    ]}
]}.
```

Now when you start up the BEAM, it should compile the protobuf files and start the `chat` app automatically!

## Writing the Client

One of the first things we can do before starting in on the client is to try downloading the generated library for Godot via your favorite HTTP tool. I'll use curl here, but you can just as easily use a web browser. With saline running, try grabbing the client library:

```bash
mkdir libsaline
cd libsaline
curl http://lcoalhost:4433/client/download > libsaline.zip 
unzip libsaline.zip
```

You should see 3 files created:
| name | description |
| ---- | ----------- |
| libsaline.gd | Auto-generated Saline client library |
| saline.proto | Wire format for Saline core functions |
| chat.proto | Wire format for the chat application |




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
