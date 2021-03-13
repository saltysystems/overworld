% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/goblet"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(goblet_account,{email, id, hash, salt, first_login, last_login, player_ids}).
-record(goblet_player,{name, id, title, appearance, role, stats, inventory, online}).
-record(goblet_entity,{id, name, stats, scripts}).
-record(goblet_object,{id, zone, coordinates}).
-record(goblet_script,{id, name, content, scope}).
-record(goblet_zone,{id, name, shortname}).

% Records representing ephemeral objects, such as matches
-record(goblet_match, {id=-1, state, players, players_max, start_time, mode, extra= <<>>}).
