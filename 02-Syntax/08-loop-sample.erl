% Erlang is a functional programming language and what needs to be remembered about all functional programming languages is that they don’t offer any constructs for loops.
% Instead, functional programming depends on a concept called recursion.


-module(loop). 
-export([]). 


% You can create a for loop with recursion
for(0, _) -> 
  ok; 
  
for(Max, Min) when Max > 0 -> 
  io:fwrite("Num : ~p\n", [Max]), 
  for(Max - 1, Min). 



while(L) -> while(L, 0). 
while([], Acc) -> Acc;
while([_|T], Acc) ->
  io:fwrite("~w~n",[Acc]), 
  while(T,Acc+1). 



loop(Map) when is_map(Map) -> 
  Keys = maps:keys(Map),
  loop(Map, Keys).

loop(_ , []) ->
  ok;
loop(Map, [Head|Tail]) ->
  Value = maps:get(Head, Map),
  io:format("~p: ~p~n", [Head, Value]),
  loop(Map, Tail).