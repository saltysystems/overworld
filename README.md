Goblet Game Server
=================================================
Goblet is an online gaming service powered by Erlang/OTP. The project
currently focuses on building tools for the creation of persistent multiplayer
worlds. Goblet intends to provide some of the basic, common capabilities
of persistent worlds including account management, player avatars, zones, items
& inventory, mechanisms for progression, NPCs, and so on. 

We are strongly influenced by massively multiplayer games of yore, such as
Ultima Online, Everquest, Star Wars Galaxies, and the many colorful creations
within platforms like BYOND and Neverwinter Nights Persistent Worlds. It is our
belief that giving agency to players to craft their own experiences will build
a vibrant community for years to come.


Getting started
-------------------------------------------------
First, install OTP from the Erlang website or your local package repository.
The framework has mostly been developed with OTP 21+. It has not been tested
with older versions.

Goblet uses the `rebar3` deployment tool heavily. It would be good to
familiarize yourself with rebar.


Style
-------------------------------------------------
Please try to annotate functions with type specs and edoc comments. This will
make dialyzer happy, it will make the maintainers happy, and it may even make
you happy.

Generally, functions may have single newlines for clarity within their
defintion, while functions themselves will have two consecutive newlines
following them for separation.


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


One very important message
-------------------------------------------------
This is Free Software under the terms of the Affero General Public License
(AGPL).  Please read, understand, and respect the terms of the license. 
