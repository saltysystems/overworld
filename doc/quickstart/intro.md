# Quick Start Introduction
Overworld is designed to interface the worlds of Erlang-based server
applications with Godot-based clients, In this quickstart, we'll put together a
simple chat application using Overworld. This application will consist of two
parts: a **server** which we will build in Erlang, and a **client** which will
be written in [Godot](https://godotengine.com/).

To get data on and off of the wire, we will implement a
[Protobuf](https://protobuf.dev/) serialization for our Chat protocol,
automatically generate client and server bindings, and run a chat application
with a few clients.

[Creating the server](server.md)
[Creating the client](client.md)
