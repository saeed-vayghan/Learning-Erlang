-module(dic).
-export([new/0, lookup/2, add/3, delete/2]).


new() -> [].

lookup(Key, [{Key, Value} | _]) -> {value, Value};
lookup(Key, [_ | Rest]) -> lookup(Key, Rest);
lookup(_, []) -> undefined.

add(Key, Value, Dict) ->
  NewDict = delete(Key, Dict),
  [{Key, Value} | NewDict].

delete(Key, [{Key, _} | Rest]) -> Rest;
delete(Key, [Pair | Rest]) -> [Pair | delete(Key, Rest)];
delete(_, []) -> [].