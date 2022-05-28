Saline Game Server
=================================================
Saline is a multiplayer game server powered by Erlang/OTP. The project is
focused on building tools for the creation of persistent multiplayer worlds,
including account management, database persistence, instances/sharding, NPCs,
server-side scripting and more.

The project draws inspiration from classic large-scale multiplayer games like
Ultima Online, EverQuest, Star Wars Galaxies, and the many amazing
user-generated worlds built with platforms like BYOND and Neverwinter Nights.

Saline is open source software, made available under the MIT license.


Getting started
-------------------------------------------------
First, install OTP from the Erlang website or your local package repository.
The framework has mostly been developed with OTP 21+. It has not been tested
with older versions.

You will also need the `rebar3` build tool in order to build, test and deploy
the server.


Style
-------------------------------------------------
Please try to annotate functions with type specs and edoc comments. This will
make dialyzer happy, it will make the maintainers happy, and it may even make
you happy.


Before creating a merge request
-------------------------------------------------
Please ensure that all proposed code changes have been run through rebar in
the following way:
```bash
  rebar3 fmt --write
```

`.hrl` files are explicitly excluded because some of the data is formatted in a
particular way there. 

Run dialyzer and try to eliminate errors to the best of your ability:
```bash
  rebar3 dialyzer
```

Ideally, any new function would have a corresponding test written for it
following the function definition.  You should validate that
`eunit:run_test(<your module>)` succeeds as expected.

You should also attempt to annotate functions with type specs as best as
possible.
