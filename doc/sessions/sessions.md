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
