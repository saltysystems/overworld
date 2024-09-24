# Quick Start

In this quickstart, we'll put together a simple chat application using
Overworld. This application will consist of two parts: a **server** which we
will build in Erlang, and a **client** which will be written in
[Godot](https://godotengine.com/).

We will implement a [Protobuf](https://protobuf.dev/) serialization for our
Chat protocol, automatically generate client and server bindings, and run a
chat application with a few clients.

## Creating the server

The first thing we'll do is build a server for the chat application. It will
broadcast messages as soon as they are received to any connected clients over a
WebSocket session.

### Server installation requirements
On the server, you will need [Erlang/OTP 27](https://www.erlang.org/downloads)
or above, as well as the [rebar3](https://rebar3.org/) tool. Earlier versions
of Erlang may not work as the `maybe` construct is used frequently in the code.

### Creating the `chat` app
First we'll create a new app called Chat. In your preferred working directory,
run the following command to instantiate the template for our new `chat`
server:

```bash
rebar3 new app chat
cd chat
```

### Setting up the rebar3 config file

From here on out, we will assume you are working from the root of the `chat`
directory. First, we'll want to add `overworld` to the list of deps in
`rebar.config`.

You will be changing this line:

```erlang
{deps, []}.
```

To look something like this:

```erlang
{deps, [
    {overworld, 
        {git, "https://github.com/saltysystems/overworld.git", {branch, "master"}}
    },
]}.

```

Once saved, you can try to grab the latest version of Overworld back in your
shell:

```bash
rebar3 upgrade overworld
```

We'll also need to add a plugin for Protobuf schema compilation, namely
[rebar3_gpb_plugin](https://github.com/lrascao/rebar3_gpb_plugin). Add the
following lines to your `rebar.conf`:

```erlang
{plugins, [
    {rebar3_gpb_plugin, "2.23.2"}
]}.

{erl_opts, [
    {i, "./_build/default/plugins/gpb/include"}
]}.

{gpb_opts, [
    % path/to/proto_dir
    {i, "priv/proto"},
    % Naming convention, unless you have a good reason just go with it.
    {module_name_suffix, "_pb"},
    % Where the generated source is placed
    {o_erl, "src"},
    % Where the generated include files are placed
    {o_hrl, "src"},
    {strings_as_binaries, false},
    {maps, true},
    {msgs_as_maps, true},
    {mapfields_as_maps, true},
    type_specs
]}.

{provider_hooks, [
    {pre, [
        {compile, {protobuf, compile}},
        {clean, {protobuf, clean}}
    ]}
]}.
```

Now you should have all of the dependencies setup! At this point you might want
to add LSP, Formatter, or other tools to your taste.

### Building a chat zone
Overworld defines a behavior called a Zone, which represents a type of game
that is continuously updated based on some periodic processing interval which
we call a 'tick'. For the chat zone, we'll start by building the skeleton of
this behavior with its required callbacks. Start a new file in the `src/`
directory, called `chat_zone.erl`. We'll include the entire file here, and then
go through the gist of it:

```erlang
-module(chat_zone).
-behaviour(ow_zone).


  -export([
           start_link/0,
           stop/0,
           join/2,
           part/2
          ]).

  -export([init/1,
           handle_join/3,
           handle_part/3,
           handle_tick/2
          ]).

  -define(SERVER, ?MODULE).

  start_link() ->
      ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

  stop() ->
      ow_zone:stop(?SERVER).

  join(Msg, SessionID) ->
      ow_zone:join(?SERVER, Msg, SessionID).
  part(Msg, SessionID) ->
      ow_zone:part(?SERVER, Msg, SessionID).

  init([]) ->
      State = #{},
      Config = #{},
      {ok, State, Config}.

  handle_join(_Msg, SessionID, State) ->
      % A player has joined
      logger:notice("Player ~p has joined the chat.", [SessionID]),
      {noreply, State}.
  handle_part(_Msg, SessionID, State) ->
      % A player has left
      logger:notice("Player ~p has left the chat.", [SessionID]),
      {noreply, State}.
  handle_tick(_ZoneData, State) ->
      {noreply, State}.
```

We first implement API functions for starting and stopping the server, as well
as joining and parting. You can add any additional logic here that you like, as
well as during the callback handler (e.g. `handle_join/3` or `handle_part/3`).

The behavior requires the callbacks `handle_join/3`, `handle_part/3`, and
`handle_tick/2`. These correspond to handling when new clients join the zone,
when clients part (leave) the zone, and when the server is supposed to process
a tick that updates its internal state. You can also implement the optinal
`handle_disconnect/2` behavior which is triggered depending on zone
configuration.

At this point we have a working zone, although it doesn't do much aside from
log when a player joins or leaves.

This also implements only the essentials of the `ow_zone` behaviour, but we can
test it out on the shell, via `rebar3 shell` in the `chat` directory:

```
 $ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling overworld
===> Analyzing applications...
===> Compiling chat
Erlang/OTP 27 [erts-15.0.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [jit:ns]

Eshell V15.0.1 (press Ctrl+G to abort, type help(). for help)
=WARNING REPORT==== 23-Sep-2024::20:48:39.537181 ===
Not starting HTTPS, no certfile provided
===> Booted chat
===> Booted mnesia
===> Booted gproc
===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted enet
===> Booted overworld
1> chat_zone:start_link().
{ok,<0.574.0>}
=NOTICE REPORT==== 23-Sep-2024::20:48:46.978825 ===
Zone server started: {ok,<0.574.0>}
2> {ok, ID} = ow_session:start(),
   chat_zone:join(#{}, ID),
   ow_session:status(connected, ID).
=NOTICE REPORT==== 23-Sep-2024::20:48:52.575007 ===
Player 3 has joined the chat.
{ok,connected}
```

Now in the same shell, you can set the session to disconnected (emulating a
client abruptly closing the connection) and the connection will timeout
gracefully and call the appropriate handlers:
```
3> ow_session:status(disconnected, ID).
{ok,disconnected}
=NOTICE REPORT==== 23-Sep-2024::20:49:27.995724 ===
Calling disconnect timeout: 3 -> <0.574.0>
=NOTICE REPORT==== 23-Sep-2024::20:49:27.995823 ===
Terminating session <0.578.0> in state disconnected after hitting timeout
=NOTICE REPORT==== 23-Sep-2024::20:49:27.995834 ===
Got disconnect timeout from session 3. My state: {state,chat_zone,#{},
                                                  #{active => [3],
                                                    disconnect => soft,
                                                    parted => [],joined => [],
                                                    frame => 2050,
                                                    tick_ms => 20,
                                                    lerp_period => 80}}
=NOTICE REPORT==== 23-Sep-2024::20:49:27.995926 ===
Player 3 has left the chat.
```

*Note*: Overworld SessionIDs are generated as monotonically increasing positive
integers. 

### Adding a serialization schema

In order to transport Overworld messages across the network to client machines,
we will use Protocol Buffers to define a schema for serialization. Right now,
we only have the ability to join and leave the server. Let's have the player
identify themselves in the join message, and define an IRC-like parting
message as well.

You'll need to make two directories, `priv` to hold applicaiton specific files,
and `priv/proto` to hold our Protobuf schemas. From the `chat` application
top-level directory:

```
mkdir -p priv/proto # This will create both priv and priv/proto if they don't exist
```

Then start a new file called `chat.proto` in that directory, with the following content:

```protobuf
syntax = "proto2";

package chat;

message chat {
    oneof msg {
        join join = 1;
        part part = 2;
    }
}

message join {
    optional string handle = 1;
}

message part {
    optional string parting_wisdom = 1;
}
```

This will allow clients to specify a handle when joining, and leave nuggets of
wisdom when they depart.

### Adding the client/server annotations

One last piece we need to add is client and server RPC annotations to the
`chat_zone` module. This will inform Overworld that it should generate client
bindings for these RPCs.

In the `chat_zone.erl` module, add the following near the top:

```erlang
-rpc_client([]). % Server -> Client
-rpc_server([join, part]). % Client -> Server
```

Now the "join" and "part" functions will have corresponding Client Library
functions automatically generated by Overworld.

### Aside: Downloading the library by hand
Overworld has a convenient HTTP API for serving a Godot client library and
corresponding Protobuf schema files. With the Erlang shell running, you can
view available files on the commandline via:

```bash
curl <Overworld server address>:4433/client/manifest
```

where your Overworld server address is something like `localhost` or your
server IP.


