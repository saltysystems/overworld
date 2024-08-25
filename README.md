Overworld Game Server
=================================================

Overworld is a multiplayer game server framework powered by Erlang/OTP. The
project is focused on building tools for the creation of player-driven worlds
of all kinds, with a special focus on account management, database persistence,
instances/sharding, NPCs, server-side scripting and more.

The project draws inspiration from classic large-scale multiplayer games like
Ultima Online, EverQuest, Star Wars Galaxies, and the many amazing
user-generated worlds built with platforms like BYOND, Neverwinter Nights, and
Roblox.

Overworld is Free Software, available under the MPL.

Getting started
-------------------------------------------------
First, install OTP from the Erlang website or your local package repository.
This framework tracks the latest release of Erlang, and is not tested on older 
versions. Notably, it makes heavy use of the `maybe` construct which requires
Erlang OTP 26 or greater.

You will also need the `rebar3` build tool in order to build, test and deploy
the server.

The documentation is sorely lacking at the moment, so feel free to open an
issue or email me at cmdrk@protonmail.com if you'd like to chat about this 
project.
