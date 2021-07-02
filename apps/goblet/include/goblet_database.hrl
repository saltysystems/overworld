% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/goblet"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(goblet_account,{email, id, hash, salt, first_login, last_login, player_ids}).
-record(goblet_player,{name, id, colors, symbols, role, health, energy, flags, inventory}).
% separate table because separate IDs
-record(goblet_mob,{name, id, appearance, type, health, energy, flags, inventory}).
-record(goblet_item, {name, id, ap, action, target_type, target_damage, target_health, status_effect, flags, price}).
