# Overworld Tutorials
These tutorials cover the basics of creating BEAM-powered game servers
connected with Godot-based game clients using the Overworld framework.

To proceed with the tutorials, you will need to have [Erlang/OTP
27](https://www.erlang.org/downloads) (or greater) as well as
[rebar3](https://rebar3.org/) installed on the system you plan to use as your
game server. For your client, we recommend the latest stable release of the
[Godot](https://godotengine.com/) 4.x release series.

## Tutorial Server Installation


Install necessary packages via package manager 

```
apt-get install curl build-essential autoconf libssl-dev libncurses-dev
```

Fetch [kerl](https://github.com/kerl/kerl) and [rebar3](https://rebar3.org/)

```
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
curl -O https://s3.amazonaws.com/rebar3/rebar3
```

Make those files executable with `chmod +x` and optionally add them to your
PATH. Now use kerl to build and install your Erlang/OTP instance

```
kerl build 27.1.2
kerl install 27.1.2 ~/erlang27.1.2
```

Activate and deactivate the Erlang environment analogously to a python venv

```
. ~/erlang27.1.2/activate
kerl_deactivate
```

Use rebar3 to build and run the server for the tutorial. Rebar3 should
automatically fetch dependencies for the overworld game server. The `shell`
subcommand will start the server and begin listening for connections:

```
git clone git@github.com:saltysystems/chat_server_example.git
cd chat_server_example
rebar3 shell
```

You should be left with an erlang shell running an overworld server. Use the
Godot client
[chat-client-example](https://github.com/saltysystems/chat-client-example) to
connect. Use `q().` to stop the server.

As a test for the server, run the following to join, send a message, and finally
leave as a Robot from the Erlang console:

```
1> {ok, S} = ow_session:start([{proxy, self()}]).
2> chat_zone:join(#{ handle => "Robot" }, S).
3> chat_zone:channel_msg(#{ handle => [], text => "sup"}, S).
4> chat_zone:part(#{}, S).
```

## Quickstart
In the quickstart tutorial, you will build a Chat server in Erlang that will
handle clients joining, parting, abruptly disconnecting, and sending chat
messages over WebSockets. You will learn how to use Overworld's `ow_zone`
Erlang behaviour, 

On the client side, we will assume some prior working knowledge of Godot and
focus on the Overworld integration. You will learn how to use the Overworld
Client Add-on to automatically download and generate a protobuf library for
communicating with the Overworld server. 

[Creating the server](server.md)

[Creating the client](client.md)

