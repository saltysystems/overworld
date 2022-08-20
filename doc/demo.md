# Building your first Overworld application: Chat

- [Building a chat server](#building-a-chat-server)
  * [Intro](#intro)
  * [Creating a new app](#creating-a-new-app)
  * [Hello, World!](#hello-world)
    + [Aside: Overworld messages](#aside-overworld-messages)
  * [Implementing the callbacks](#implementing-the-callbacks)
  * [Testing it out so far](#testing-it-out-so-far)
  * [Saving state and sending messages](#saving-state-and-sending-messages)
    + [Aside: Maps](#aside-maps)
  * [Serializing messages](#serializing-messages)
  * [Some finishing touches](#some-finishing-touches)
- [Building a chat client](#building-a-chat-client)
  * [Overworld and Godot](#overworld-and-godot)

## Building a chat server

### Intro

Chat applications are a familiar way to let us dive directly into building a
useful program that can be published to the world and have people and machines
connect and interact with it, while highlighting some of the features and
workflows of Overworld.

### Additional resources

Here are some links to additional resources:
  - [Overworld Chat server sources](https://github.com/saltysystems/chat) (sources for this tutorial) 
  - [Overworld Client plugin for Godot](https://github.com/saltysystems/overworld_client)

### Creating a new app

Overworld should be usable as an Erlang app in your own project. Let's start a
new `rebar3` application called "chat":

```bash
$ rebar3 new app chat
```

This should create some basic directory structure and a few files to get
started. You'll want to edit `rebar.config` and include Overworld as a
dependency. Here we add Overworld pulling from the GitHub master branch, in
`chat/rebar.config`, you'll want to change the `deps` option to look like this:

```erlang
{deps, [
  {overworld, {git, "https://github.com/saltysystems/overworld.git", {branch, "master"}}}
  ]
}.
```

You should be able to successfully `rebar3 compile` and see the library pulled
down and built. 

While you're in this config file, you'll also want to change the `apps` section under `shell` to boot Overworld and its dependencies. It should look a bit like this:
```erlang
{shell, [
  % {config, "config/sys.config"},
    {apps, [
           chat,
           overworld
           ]}
]}.
```

### Hello, World!
Make a new Erlang source file called `chat_global.erl` in the directory `src`.

You'll want to specify that this is a `ow_zone` server and export the minimum
required callback functions:
```erlang
-module(chat_global).
-behaviour(ow_zone).

% Required ow_zone callbacks
-export([
         init/1,
         handle_join/4,
         handle_part/3,
         handle_rpc/5,
         handle_tick/2
        ]).
```

The callbacks are standard functions that the Overworld application will expect
your applictaion to provide, in order to fit the behaviour of an "overworld
zone". Generally, Overworld will call your application to handle the fine
details of joining and leaving zones, handling remote procedure calls (RPCs)
from clients, and advancing the world state (ticks).

The init function and handlers form the private API of our app, to be used by
Overworld internally. Next let's start to define the public API for our Chat
server, which can be used by other parts of our program or an operator in the
shell.

We'll need some way to start and stop the server (start/0, stop/0), a way for
players to join the session (join/2), a way for players to leave (part/1), and
a way for players to send some input to the server (send/2). For each function
here, `ow_zone` will call back to our required handler functions defined above.
This will allow us to expose a standard API to our clients and hide the
implementation details. Overworld will seamlessly handle messages sent over the
network to remote clients (via Protobuf) or to other Erlang processes
(including distributed Erlang!).

Overworld will automatically keep track of any sessions connected to our world
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
    ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    ow_zone:stop(?SERVER).

join(Msg, Session) ->
    ow_zone:join(?SERVER, Msg, Session).

part(Session) ->
    ow_zone:part(?SERVER, Session).

send(Msg, Session) ->
    ow_zone:rpc(?SERVER, chat_msg, Msg, Session).
```

You may have noticed that for the send/2 function, we send along the atom
`chat_msg` - this atom will ultimately refer to the name of one of the messages
we'll define in our wire protocol. 

Now that we have defined the API boilerplate, we can start writing some
application code! The first thing we need to do is setup the initial ow_zone
server state. By default, ow_zone will let anyone connect without
authentication and will update the game world at 30 ticks per second. This
should be totally OK for our purposes. 

Normally for a game we'll want to define some initial state and mutate that
state over time as players interact with the game world in fun and exciting
ways. For a chat application, we'll use the server state to buffer all messages
received every tick, and then dump the contents of the buffer to all connected
players every tick. Let's just make the initial state an empty list for now.

```erlang
% Required callbacks 
init([]) ->
    InitialState = [],
    {ok, InitialState}.
```

Next we'll need to define what happens when players join or leave the game.
Overworld will automatically keep track of any sessions connected to our world
server and will handle message serialization/deserialization between the
clients and the server. So all you need to write is the game logic!

#### Aside: Overworld messages
>At the end of any required callback function, you'll want to return a 3-tuple
>to `ow_zone` with the following rules:
>```erlang
>    {Status, Response, State} when
>        Status :: ok | {ok, Session},
>        Response :: noreply 
>                    | {'@zone', term()}
>                    | {'@', list(), term()},
>        State :: term().
>```
>
>In plain language: for `Status` messages, you want to either respond `ok` or
>respond `ok` with an updated copy of the player's session. You may update the
>player's session if there's something about it that your callback function
>changes. Perhaps you wanted to set some game specific information in via
>`ow_session:set_game_info/1`, for example.
>
>For responses, `ow_zone` understands 3 different response messages from a
>server implementing the behaviour. You can use `'@zone'` to broadcast a message
>to everyone in the zone, in our case everyone connected to the World Server.
>You can also target individuals with `{'@', PlayerIDs}` where PlayerIDs is a
>list of player IDs that should receive this message. Lastly, you can use
>`noreply` to silently accept the message with no updates sent out to connected
>players. Here's a handy reference table:
>
>|  Response       | Description |
>| --------------- | ----------- |
>| `{'@zone', Msg}` | Send a zone-wide message *Msg* to all connected clients |
>| `{'@', PlayerID, Msg}` | Send a message, *Msg*, to player *PlayerID*. Also accepts a list of Player IDs |
>| `noreply` | Send no reply to any connected player |
>
>Finally, you should return your internal state to `ow_zone` via `State`.

### Implementing the callbacks
Now we should implement the handler functions for our chat server. We'll add
some logger business to give us some idea of what's going on, but otherwise do
nothing very exciting.

```erlang
handle_join(_Msg, Session, _Players, State) ->
    ID = ow_session:get_id(Session),
    logger:notice("Player ~p has joined the server!", [ID]),
    {ok, noreply, State}.

handle_part(Session, _Players, State) ->
    ID = ow_session:get_id(Session),
    logger:notice("Player ~p has left the server!", [ID]),
    {ok, noreply, State}.
```

For RPCs, we know we have one type function that a client might call:
`chat_msg`. Let's say that `chat_msg` will include, obviously, the message.

Notice that the function signature is a bit different for `handle_rpc` - we
must additionally include the message type. This will be especially important
when we start serializing the data over a WebSocket, because we'll need to
instruct Overworld what Protobuf message to use.

We'll take the message verbatim and stuff it into the zone state. In a real
application, you may want to modify the message before buffering it! For 
example, you may want to parse it for slash commands, filter it for bad 
words, or add/remove keys from the message map.

```erlang
handle_rpc(chat_msg, Msg, Session, _Players, State) ->
    ID = ow_session:get_id(Session),
    logger:notice("Player ~p has sent a chat message: ~p", [ID, Msg]),
    {ok, noreply, State}.
```

There are a few more callbacks that we need to define to properly implement the
`ow_zone` behavior. Since the game server is going to update every 30ms, we
have to implement logic for that update. For now we'll do the minimum necessary
to get the server to compile:

```erlang
handle_tick(_Players, State) ->
    {ok, noreply, State}.
```

At this point, everything required by Overworld is implemented. We also defined
a public API to nicely wrap joining/leaving, sending messages, and
starting/stopping the server.

### Testing it out so far 
You can save up your work at this point and try things out. Make sure you run 
`rebar3 shell` from the root of your application directory.

```erlang
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling ow
===> Compiling chi
Erlang/OTP 25 [erts-13.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns] [dtrace] [sharing-preserving]

Eshell V13.0  (abort with ^G)
===> Booted gproc
===> Booted mnesia
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted ow
===> Booted sasl
1> 
```

From the shell you can start the world server and verify everything is working:
```erlang
1> chat_global:start().
{ok,<0.482.0>}
2>
3> chat_global:stop().
ok
```

Let's also try to join the server as a player from the shell. First we need to
create a Overworld session. Whenever a connection is first established to an
Overworld server, the client handler will create a session that looks much like
the below for a user. We'll mimick it by calling the new session function
directly:
```erlang
8> S = ow_session:new().
{session,-576460752303423295,undefined,none,false,0,
         undefined,undefined}
```

We can then join and part the server with this session information. Remember
that join/2 requires a message, so let's assume that's the handle of the player
joining. When we work with protobuf later, we'll see that we mostly deal with
messages in terms of maps. So let's sketch out what that message might look
like:

```erlang
8> Name = #{ name => "rambo" }.
9> chat_global:join(Name, S).
2022-06-22T21:21:11.872998-05:00 : notice: Player -576460752303423295 has joined the server!
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
```

It works! Try sending a chat message:
```erlang
10> 
6> chat_global:send("Hello, world!", S).
P2022-06-22T21:21:15.229092-05:00 : notice: Player -576460752303423295 has sent a chat message: "Hello, world!"
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,#{},undefined}}
```

Finally, let's try leaving:
```erlang
11> chat_global:part(S).
2022-06-22T21:21:21.243792-05:00 : notice: Player -576460752303423295 has left the server!
{ok,{session,-576460752303423295,undefined,undefined,false,
             0,undefined,undefined}}
```


### Saving state and sending messages
Now that we've confirmed that the server is up and running, and we're logging
some semi-useful messages, we actually need to save and buffer those messages
and send them to connected players. The first thing that we'll need to do is
add messages to the state. Let's modify the `handle_rpc` function to do that:
```erlang
handle_rpc(chat_msg, Msg, Session, Players, State) ->
    ID = ow_session:get_id(Session),
    % Make sure the player has actually joined the zone
    State1 = 
        case ow_zone:is_player(ID, Players) of 
            false ->
                State;
            true ->
                [ #{ who => ID, msg => Msg } | State ]
        end,
    {ok, noreply, State1}.
```

A few things going on here. First, we change `_Players` to `Players` in the
function signature, so the variable is actually bound. Then, we use that
variable to check if the player sending the message is actually in the zone. If
so, we append the sender's ID and the message to the zone state and buffer it.
We don't actually send any data here, just store it for the tick function to
handle it later.

#### Aside: Maps
>You may be wondering why we're using maps to store all the data. The reason is
>pretty simple - if we ever want send data over the wire via Protobuf, using a
>map is the most flexible way to serialize it.

Now we'll need to have the zone send all of the messages it has buffered up in
each tick. To do so, we'll change the `handle_tick` function. First, we might
as well not send any updates to players when there are no messages to send. So
let's first head that off with a pattern match for empty state where we don't
reply anything, then follow it up with a zone-wide broadcast when we have
something to send.


```erlang
handle_tick(_Players, State = []) ->
    {ok, noreply, State};
handle_tick(_Players, State) ->
    % Replace the current state with an empty list and send out the state.
    State1 = []
    Reply = {'@zone', {state_transfer, State}},
    {ok, Reply, State1}.
```

Let's try it out in the shell. We've got a number of things to do here if 
we have a fresh shell:
  1. Start a new session
  2. Set the PID of the session to match our shell
  3. Start the chat server
  4. Join the server with our shell
  5. Send a message
  6. Finally, see if we get a response back.

#### Note: The value of self() can change if you make a mistake!
> If you flub one of the commands in the next section, be aware that
> the Erlang shell may crash and be restarted automagically. This will 
> change the value of `self()`! 
> You may need to re-update your session via `f(S1)` to unbind S1, and
> then re-run step #2 before proceeding. 

So we'll proceed exactly along those lines:
```erlang
1> S = ow_session:new().
2> S1 = ow_session:set_pid(self(), S).
{session,-576460752303423390,<0.538.0>,undefined,false,0,
         undefined,undefine}
3> chat_global:start().
{ok, <0.541.0>}
3> Name = #{ name => "rambo" }.
5> chat_global:join(Name, S1).
2022-06-25T09:42:03.467787-05:00 : notice: Player -576460752303423390 has joined the server!
{ok,{session,-576460752303423390,<0.538.0>,undefined,false,
             0,undefined,undefined}}
6> Msg = #{text => "hello world!"}.
6> chat_global:send(Msg, S1).
2022-06-25T09:43:43.451675-05:00 : notice: Player -576460752303423390 has sent a chat message: #{text => "hello world!"}
{ok,{session,-576460752303423390,<0.538.0>,undefined,false,
             0,undefined,undefined}}
7> flush().
Shell got {<0.541.0>,zone_msg,
           {state_transfer,[#{msg => #{text => "hello world!"},
                              who => -576460752303423390}]}}
ok
```

### Serializing messages
If we ever want players to connect from the outside world, we'll need to define
some messages that we can serialize and send across the wire. Overworld supports
[protobuf](https://developers.google.com/protocol-buffers) as a standard wire
format to communicate between game server and client.

Go ahead and make a `priv/proto` directory in the root of your chat
application, e.g., 
```bash
mkdir -p ow/apps/chat/priv/proto
```

Fire up your favorite editor and we'll start defining the protobuf file. I'll 
call it `chat.proto`.

```protobuf
syntax = "proto2"; 

package chat;
```

It's important that the Protobuf package name matches the name of the 
application for the Protobuf plugin to auto-generate the 
serializer/deserializer code. 

Now we can start defining some messages! We know for sure we need a "join"
message as well as a corresponding "part" message, as those are required
callbacks for Overworld:
```protobuf
message join {
    required string handle = 1;
}

message part {
    required string handle = 1;
}
```

The next two messages have to do with receiving chats and sending them out to
clients in batches. The first one should be pretty self-explanatory: we want to
send along the player's chat message. The `state_transfer` message will simply
tell the client to expect a series of `chat_messages` as a list, since our 
`handle_tick/2` function sends them out in batches.

```protobuf
message chat_msg {
  required string text = 1;
}

message state_transfer {
  repeated chat_msg msgs = 1;
}
```

That's it for the Protobuf file!

We have one last thing to do before Overworld will automatically encode/decode
messages for us. We need to define a callback in our server, `rpc_info/0` that
will tell Overworld some important information about our messages. First, 
let's add it to our exports list with the other callbacks:

```erlang
-export([
         init/1,
         handle_join/4,
         handle_part/3,
         handle_rpc/5,
         handle_tick/2
         rpc_info/0
        ]).
```

Now we can work on defining the RPCs. First, we define some opcodes which are
actually just a couple of bytes at the beginning of the message that let's the
client and Overworld determine which function to use to decode or encode the
message. Overworld reserves 0x0 through 0xFFF for itself by convention, but
you can use anything from 0x1000 through 0xFFFF for your own opcodes. 

Let's take a look at what the final product looks like, and then break down 
each section:

```erlang
% Overworld RPCs
-define(CHAT_JOIN, 16#1001).
-define(CHAT_PART, 16#1002).
-define(CHAT_SEND, 16#1003).
-define(CHAT_XFER, 16#1004).

rpc_info() ->
    [
        #{
            opcode => ?CHAT_JOIN,
            c2s_handler => {?MODULE, join, 2},
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_PART,
            c2s_handler => {?MODULE, part, 1},
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_SEND,
            c2s_handler => {?MODULE, send, 2},
            c2s_proto => chat_msg,
            encoder => chat_pb
        },
        #{
            opcode => ?CHAT_XFER,
            s2c_call => state_transfer,
            encoder => chat_pb
        }
    ].
```

I prefer to stick the `rpc_info/0` function near the top of the source file,
just before `start/0`. 

For messages that are received by the server, we need to define a
client-to-server handler, or `c2s_handler`. This should be a 3-tuple with the
module, function, and arity (MFA) that Overworld will call whenever it receives a
message prefixed by the appropriate opcode. Since `join`, `part,` and `send`
are messages that come from the client, we inform Overworld that they will have
corresponding `c2s_handler` functions when messages come in.

One thing that Overworld will do to determine how to associate your protobuf
messages with callbacks is inspect the name of your handler and assume that the
protobuf message has the same name. For the `send/2` function, it doesn't seem
all that great to call our messages "send", I'd much rather have a name like
`chat_msg` - where "send" is the verb and "chat_msg" is the object being sent.
So here I add the `c2s_proto` option to instruct Overworld to use the "chat_msg"
definition in our Protobuf file instead of looking for "send".

For the remaining message, we don't expect clients to send `state_transfer`
messages to our server since it represents a bundle of messages to be
dispatched all at once. So here we don't have a `c2s_handler`, but instead
define a server-to-client call, or `s2c_call`. The corresponding function for
our client will be created automatically by the Godot plugin.

### Some finishing touches
Our chat server is in pretty good shape, but let's add a few finishing touches
to make it easier once we get to wiring up the client. The chat server really
ought to start automatically whenever you fire up the BEAM, and it will also
need to call into Overworld to register its opcodes and associated callback
functions.

To register our app with Overworld, open up `chat_app.erl` and add 
```erlang
  ow_protocol:register_app(chat)
```
to the `start/2` function, like so:

```erlang
-module(chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ow_protocol:register_app(chat),
    chat_sup:start_link().

stop(_State) ->
    ok.
```

We also need to make sure that the supervisor starts up our Chat server. You'll
need to edit `chat_sup.erl` and add a child spec for the chat server in the
`init/1` function:

```erlang
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

You will also want to make sure to edit the `rebar.config` for your application
to set up proper encoding and decoding of Protobuf messages:

```erlang
{plugins, [
  {rebar3_gpb_plugin, "2.13.2"},
]}.

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

Now when you start up the BEAM, it should compile the protobuf files
and start the `chat` app automatically!

Lastly, we need to fix up the `chat.app.src` file to be a bit more 
descriptive, and to ensure that `overworld` gets started before `chat`, 
so the Chat app only attempts to register itself _after_ Overworld 
starts up. Mine looks like this:

```erlang
{application, chat,
 [{description, "An Overworld application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {chat_app, []}},
  {applications,
   [kernel,
    overworld,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["MIT"]},
  {links, []}
 ]}.
```

Once you've added all of the necessary bits and bobs, you can try starting
up a shell again and ensure that the Chat app is registered and running:

```erlang
1> ow_protocol:registered_apps().
[chat,ow]
2>  regs().

** Registered procs on node nonode@nohost **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <0.44.0>     erlang:apply/2                  253501    0
chat_global           <0.389.0>    ow_zone:init/1                   79643    0
chat_sup              <0.388.0>    supervisor:chat_sup/1              185    0
...
```

## Building a chat client
### Intro

The [Overworld Client plugin](https://github.com/saltysystems/overworld_client) 
is able to connect to a running Overworld server from within Godot and generate 
a GDScript-based library for marshalling game data from Protobuf objects into 
GDScript types and back again.

#### Aside: Inspecting the client zip file
>With Overworld running, try downloading the client library and extracting it 
>to some local directory:
>
>```bash
>mkdir libow
>cd libow
>curl http://localhost:4433/client/download > libow.zip 
>unzip libow.zip
>```
>
>If you have Overworld running on some other computer on your network, just 
>change _localhost_ to your server's IP.
>
>After you unzip the library, you should see 3 files created:
>| name | description |
>| ---- | ----------- |
>| libow.gd | Auto-generated Overworld client library |
>| ow.proto | Wire format for Overworld core functions |
>| chat.proto | Wire format for the chat application |
>
>The `ow.proto` file will contain various objects needed for Overworld itself,
>while `chat.proto` should be identical to the one you wrote earlier. 
>`libow.gd` will contain all of the high-level code needed to facilitate 
>communication between your game or application and Overworld. 

### Creating a new Godot project

This isn't meant to be a comprehensive tutorial on building applications with
Godot. However, you can start by making a new project called "chat_client".

### Installing the Overworld client
Either clone the Overworld client plugin repo, or download a compressed release
from GitHub, and then copy the `addons` directory into your Chat application
directory.

```bash
git clone https://github.com/saltysystems/overworld_client
cp -a overworld_client/addons chat_client/addons
```

You'll want to enable the plugin by going to `Project -> Project Settings... ->
Plugins` and clicking the checkbox next to `Enable`. 

You should see a new tab in your Godot editor, labeled "Overworld". 

### Downloading and compiling your custom Overworld library

Once you have the Client plugin installed, you'll want to point it at your 
Overworld server (localhost, if on the same computer, otherwise the server's 
IP or hostname). Click the "Download & Compile" button for the plugin to 
download the client zip file from your server, extract it and compile the 
Protobuf files from sources. 

You will want to ensure that the output directory has been created prior to
trying to compile the Overworld library, otherwise you will receive an error
message.

### Autoloading the client

You will probably want to autoload the Overworld client in your scenes to
facilitate network communication. Go to `Project -> Project Settings... ->
AutoLoad`. Under `Path:`, you'll need to type in the path to libow.gd or click
the folder icon to locate it. On the right, you'll need to give the Network library a node name. I typically call it `NetworkClient`. 

Once that's done, make sure to click the `Enable` checkbox under Global
Variable and close the project settings.



## Troubleshooting

### Server Server

### Client Setup

