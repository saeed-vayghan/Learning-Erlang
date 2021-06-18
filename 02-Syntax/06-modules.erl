% -Name(Attribute)

% The leading ‘-’ is called the attribute prefix.

-module(ModuleName)
% The ModuleName needs to be same as the file name minus the extension .erl.
% -module(Name). This is always the first attribute of a file. (Name is an atom)

% -export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).
% Exported functions represent a module's interface. 
-export([dir/0, dir/2]).

% -import(Module, [Function1/Arity, ..., FunctionN/Arity]).
-import(io, [fwrite/1]).

-compile([debug_info, export_all]).
-compile(export_all).

-define(EXT, ".erl"). % file extension to look for
-define(MODS, "./").
-define(TESTS, "./tests/").

-author("An Erlang Champ")
