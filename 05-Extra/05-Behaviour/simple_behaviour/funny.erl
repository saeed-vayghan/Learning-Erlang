-module(funny).

-behaviour(gen_funny).
-export([foo/1]).

foo(Int) ->
  integer_to_list(Int).