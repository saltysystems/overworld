% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/goblet"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(goblet_account,{email, id, hash, salt, first_login, last_login, player_ids}).
-record(goblet_player,{name, id, colors, symbols, role, health, status_effects, inventory}).
% separate table because separate IDs
-record(goblet_entity,{name, id, appearance, role, health, status_effects, inventory}).
-record(goblet_item, {name, id, ap, action, target_type, target_damage, target_health, status_effect, price}).
