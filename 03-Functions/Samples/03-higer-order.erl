-module(hofuns).
-compile(export_all).
 
one() -> 1.
two() -> 2.
 
add(X,Y) -> X() + Y().


increment([]) -> [];
increment([H|T]) -> [H+1 | increment(T)].
 
decrement([]) -> [];
decrement([H|T]) -> [H-1 | decrement(T)].


map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].
 
incr(X) -> X + 1.
decr(X) -> X - 1.



% 2> L = [1,2,3,4,5].
% [1,2,3,4,5]
% 3> hofuns:increment(L). 
% [2,3,4,5,6]
% 4> hofuns:decrement(L). 
% [0,1,2,3,4]