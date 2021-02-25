% Erlang is a functional programming language and what needs to be remembered about all functional programming languages is that they don’t offer any constructs for loops.
% Instead, functional programming depends on a concept called recursion.


-module(loop). 
-export([while/1, while/2, for/2, start/0]). 


while(L) -> while(L, 0). 
while([], Acc) -> Acc;
while([_|T], Acc) ->
  io:fwrite("~w~n",[Acc]), 
  while(T,Acc+1). 




for(0, _) -> 
  done; 

for(N, Term) when N > 0 -> 
  io:fwrite("Hello~n"),
  for(N-1, Term).





% start() -> 
%   X = [1,2,3,4], 
%   while(X).

start() -> 
  for(5,1).
