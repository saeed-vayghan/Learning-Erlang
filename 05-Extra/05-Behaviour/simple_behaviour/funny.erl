-module(funny).

-behaviour(gen_funny).
-export([foo/1]).

foo([]) ->
  some_atom.