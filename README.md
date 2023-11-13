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
The framework has mostly been developed with OTP 21+. It has not been tested
with older versions.

You will also need the `rebar3` build tool in order to build, test and deploy
the server.

You can consult the [documentation](doc) directory for some examples on using
Overworld. The [Chat application demo](doc/demo.md) is a good place to get
started.
