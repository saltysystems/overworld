# Overworld Tutorials
These tutorials cover the basics of creating BEAM-powered game servers
connected with Godot-based game clients using the Overworld framework.

To proceed with the tutorials, you will need to have [Erlang/OTP
27](https://www.erlang.org/downloads) (or greater) as well as
[rebar3](https://rebar3.org/) installed on the system you plan to use as your
game server. For your client, we recommend the latest stable release of the
[Godot](https://godotengine.com/) 4.x release series.


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
