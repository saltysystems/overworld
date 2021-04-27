Goblet Game Server
=================================================
Goblet is a multiplayer game server powered by Erlang/OTP. The project is
focused on building tools for the creation of persistent multiplayer worlds,
including account management, player progression, zones, items & inventory,
NPCs, scripting and more.

The project draws inspiration from classic large-scale multiplayer games like
Ultima Online, EverQuest, Star Wars Galaxies, and the many colorful and
creative worlds built with platforms like BYOND and Neverwinter Nights.

A strategy game, "Known Space", is being built alongside Goblet as an early game using the service.

Goblet is Free Software, licensed under the Affero General Public License v3. 
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
particular way there. If anyone has a way to mark sections as 'no format' I
would love to know.

Run dialyzer and try to eliminate errors to the best of your ability:
```bash
  rebar3 dialyzer
```

Any new function will need to have a corresponding test written for it in
`apps/goblet/src/tests`. You should validate that `eunit:run_test(<your
module>)` succeeds as expected.

You should also attempt to annotate functions with type specs as best as
possible.
