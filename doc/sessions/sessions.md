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
zone. To start a session, you will need to call the session supervisor via
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
messages as normal.

This sort of functionality is useful when implementing server-side agents
(i.e., non-player characters or NPCs). For example, one could to write a module
that implements either the `gen_server` or `gen_statem` behaviour and creates a
new session as part of its `init/1` callback, handles events (i.e. via
`handle_info/2`), and responds appropriately.

## Session discnnects
Given that network connections cannot be assumed to be perfectly stable,
sessions in Overworld persist independent of the connection handler (or
_proxy_) to potentially give clients an opportunity to reconnect to an existing
session. The Overworld [Zone](../zone/zone.md) behaviour allows for two types of disconnects:
* **soft**: The `handle_disconnect/2` function is called on the game module
  implementing the Zone behaviour. The implementor may choose to forward this
  information to other clients, update some internal state, or do nothing at
  all. The client is removed from the zone after the `disconnect_timeout` is
  reached.
* **hard**: The `handle_part/3` function is called on the game module and the
  client is immediately removed from the zone. The session persists for
  `disconnect_timeout` milliseconds and then shuts down.
