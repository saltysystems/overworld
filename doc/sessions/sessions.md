# Sessions
Every client that connects to an Overworld server first establishes a
_session_. The client session is independent of the network connection and is a
stateful process that holds a number of important pieces of metadata about the
client.

Sessions have the following properties:
* **id**: A numeric identifier, usually a positive integer
* **proxy**: An Erlang process id that will receive messages meant for the
  session. Usually a WebSocket or ENet handler, but can be any Erlang process.
* **serializer**: The method used to serialize data for a remote procedure
  calls. Either `protobuf`, for connections over WebSockets/ENet, or
  `undefined` for messages sent to/from other Erlang processes. In the latter
  case, messages will be sent as native tuples. 
* **latency**: The most recent measure of round trip time between
  client/server, in milliseconds. Only relevant for ENet/WebSocket connections.
* **disconnect_callback**: The module, function, and argument list that will be
  called when a disconnect is detected.
* **disconnect_timeout**: The time (in milliseconds) before a timeout is triggered (default 5000ms).
* **status**: The connection state. Can be one of `preconnect`, `connected`, or `disconnected`.
* **token**: Randomly generated binary data that is presented to the client on
  first connection, and can be used to assert control over a session after
  reconnecting from an aborted connection.
* **zone**: The Erlang PID of the Zone that the session is currently participating in.
* **game_data**: Arbitrary game data set by the application, can be any valid Erlang term.

## Starting a session
Sessions are usually started automatically by WebSocket or ENet handlers, but
in some cases it may be desireable to start a session manually and join it to a
zone. For example, if you wanted to create a non-player character (NPC) that
acts as an autonomous agent in a zone, you start a session for that NPC and
implement all of the relevant message handlers.

To start a session, you will need to call the session supervisor via
`ow_session_sup:new/1` and give it some
[proplist](https://www.erlang.org/docs/26/man/proplists) corresponding to the
properties of the session process listed in the previous section. For example,
you could create a session and connect it back to your shell: 

```erlang
1> Config = [
    { proxy, self() },
    { status, connected }
   ].
[{proxy,<0.461.0>},{status,connected}]
2> ow_session_sup:new(Config).
{ok,<0.536.0>}
```

This session would then be able to join any running zone and send/receive
messages as normal. An NPC could be implemented by creating a `gen_server` or
`gen_statem` that creates a new session as part of its `init/1` callback and
then handles events (i.e. via `handle_info/2`) and responds appropriately.
