% Lets imagine this very file's name is testing.erl

-module(testing).

-export([dir/0, dir/2]).

-import(io, [fwrite/1]).

-compile([debug_info, export_all]).

-define(EXT, ".erl"). % file extension to look for
-define(MODS, "./").
-define(TESTS, "./tests/").

-author("An Erlang Champ")


% Logic goes here