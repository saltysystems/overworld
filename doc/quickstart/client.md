# Creating the client

With server in hand and running in a `rebar3 shell`, you can start hacking away
at a client. Since we're more focused on the server-side programming, we will
assume some Godot knowledge and provide a client template repo from which you
can start.

## Downloading the Chat Client example repository
Download the zip, or clone the Chat Client example repository to a location of
your choice, e.g.:

```bash
cd ~ # to cd to $HOME, for example
git clone https://github.com/saltysystems/chat-client-example
```

Open Godot and click **Import**. Navigate to this project directory and select
the chat-client-example directory.

## Installing the Overworld Client Add-on
The [Overworld Client Add-on](https://github.com/saltysystems/overworld_client)
is a Godot plugin that provides a new tab in your Godot editor to download and
compile Protobuf files using [Godobuf](https://github.com/oniksan/godobuf). 

You'll want to clone or download a zip of this archive from GitHub, and then
copy/paste the Add-Ons directory into your `chat-client-example` directory from above.

From a shell:
```bash
cd ~ # to cd to $HOME, for example
git clone https://github.com/saltysystems/overworld_client
cp -a overworld_client/addons chat-client-example/addons
```

## Enabling the Overworld Client
In your Godot editor, click **Project**, then **Project Settings**, choose the
**Plugins** tab. You should see the "Overworld Client" listed. Click the **On**
checkbox and then click **Close**.

Now you should see an new tab in your editor called **Overworld**. This is how
you'll interact with the Overworld server, download protobuf files and generate
the client library on-the-fly.

## Generating the Chat client library

First, if you haven't read though the [Creating the server](server.md)
documentation, please do that now. Before you continue, you'll want to have the
Overworld server running in a `rebar3 shell` or otherwise. 

Enter your Overworld server address. If the server is running on the same
computer as your client, the defaults should be fine. Otherwise you'll need to
adjust the _Overworld Server Address_ field as appropriate.

By default, Overworld will download all files to `res://scripts` directory in
your Godot project. If this directory doesn't exist, create it now or choose a
directory by clicking on the ellipsis (`...`) below _Output Directory_ in the
Overworld tab of Godot.

Once you have set up the server address and created or chosen an appropriate
scripts directory, click **Download & Compile!**. 

On the server, you should see something like this in the logs:
```
=INFO REPORT==== 27-Sep-2024::10:20:51.299144 ===
Client "192.168.1.100" GET manifest.json
=INFO REPORT==== 27-Sep-2024::10:20:51.437124 ===
Client "192.168.1.100" GET "libow4.gd"
=INFO REPORT==== 27-Sep-2024::10:20:51.596506 ===
Client "192.168.1.100" GET "overworld.proto"
```

On the client, you should see the `scripts/` directory populated with 2 files:
`libow4.gd` and `overworld_pb.gd`
