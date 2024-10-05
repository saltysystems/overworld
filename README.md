Overworld Game Server
=================================================

Overworld is a multiplayer game server framework for Erlang and other BEAM
languages, designed to seamlessly integrate with the [Godot Game
Engine](https://godotengine.org/). 

Description
-------------------------------------------------
Overworld provides developers with a toolkit to build scalable, multiplayer
games more efficiently. It manages several low-level aspects of network
communication, including:

1. Network connections via TCP (WebSocket) or UDP (ENet), or freely mix the two!
2. Serialization and deserialization of data to and from wire format (via Protobuf)
3. Client code generation for Godot from low-level packet handling to signaling
   game events 

Abstracting the networking details, Overworld allows developers to concentrate
more on game logic and less on communication infrastructure. This framework is
suitable for various multiplayer game types, from small co-op experiences to
larger online games.

Key features of Overworld include:

- Tight integration with Godot Game Engine
- Support for games written in both Erlang (_Elixir coming soon_)
- Tick-based game loop management

Overworld aims to provide a solid foundation for multiplayer game development,
streamlining the process of creating networked games while leveraging the
robustness of Erlang/OTP.

License
-------------------------------------------------
Overworld is released under the MPL 2.0 License. See the LICENSE file for more
details.

Prerequisites
-------------------------------------------------
* [Erlang/OTP 27](https://www.erlang.org/downloads) or greater
* [rebar3](https://rebar3.org/)

Quickstart
-------------------------------------------------
See [here](docs/quickstart/intro.md)

Roadmap
-------------------------------------------------
* Elixir support
* Optional Lua-based game logic, via [Luerl](https://github.com/rvirding/luerl)
* More docs and demos! :sweat_smile:


