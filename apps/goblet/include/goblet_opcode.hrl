-define(PROTOCOLVERSION, 1).

% First two bytes of a message are its opcode which maps onto RPCs supported by and informational messages generated by the server

% General commands
-define(VERSION, 16#1000).
-define(HEARTBEAT, 16#1010).
-define(ACCOUNT_NEW, 16#1040).
-define(ACCOUNT_LOGIN, 16#1045).

% Player management commands
-define(PLAYER_NEW, 16#1050).
-define(PLAYER_LIST, 16#1055).
-define(PLAYER_LOG, 16#1060). % chat messages

% Lobby and match creation commands
-define(MATCH_LIST, 16#2010).
-define(MATCH_CREATE, 16#2020).
-define(MATCH_JOIN, 16#2030).
-define(MATCH_LEAVE, 16#2040).
-define(MATCH_START, 16#2050).
-define(MATCH_INFO, 16#2060).
-define(MATCH_STATE, 16#2070).
-define(MATCH_PREPARE, 16#2080). % acknowledge match ready
-define(MATCH_DECIDE, 16#2082).  % send decision
-define(MATCH_EXECUTE, 16#2084). % cue to play animations
-define(MATCH_FINISH,16#2086). % show summary screen
