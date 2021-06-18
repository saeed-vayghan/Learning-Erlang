-module(functions).
-compile(export_all).


%% Simulate foreach
foreach(Fun, [H|T]) ->
  Fun(H),
  foreach(Fun, T);
foreach(_Fun, []) ->
  ok.


%% Simulate map
map(Fun, [H|T]) -> [Fun(H) | map(Fun,T)];
map(_Fun, []) -> [].


% Simulate for and while
for(0, _) -> ok; 
for(Max, Min) when Max > 0 -> for(Max - 1, Min). 


while(L)          -> while(L, 0). 
while([], Acc)    -> Acc;
while([_|T], Acc) -> while(T,Acc+1). 


%% Simulate loop

loop(Map) when is_map(Map) -> 
  Keys = maps:keys(Map),
  loop(Map, Keys).

loop(_ , []) -> ok;
loop(Map, [Head|Tail]) ->
  Value = maps:get(Head, Map),
  io:format("~p: ~p~n", [Head, Value]),
  loop(Map, Tail).