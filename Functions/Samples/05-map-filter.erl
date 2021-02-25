-module(hofuns).
-compile(export_all).


map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].
 
incr(X) -> X + 1.
decr(X) -> X - 1.



L = [1, 2, 3, 4].
Base = 2.
PowerOfTwo = fun(X) -> math:pow(Base, X) end.
hofuns:map(PowerOfTwo, L).
% [2.0,4.0,8.0,16.0]


hhfuns:map(fun(X) -> X + 1 end, L).
% [2,3,4,5,6]

hofuns:map(fun hofuns:incr/1, L).  
% [2,3,4,5,6]




%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L,[])).
old_men([], Acc) -> Acc;
old_men([Person = {male, Age} | People], Acc) when Age > 60 -> old_men(People, [Person|Acc]);
old_men([_|People], Acc) -> old_men(People, Acc).





%% only keep even numbers
even(L) -> lists:reverse(even(L,[])).
even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> even(T, [H|Acc]);
even([_|T], Acc) -> even(T, Acc).





filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.

Numbers = lists:seq(1,10).
% [1,2,3,4,5,6,7,8,9,10]

hhfuns:filter(fun(X) -> X rem 2 == 0 end, Numbers).
% [2,4,6,8,10]

People = [{male,45}, {female,67}, {male,66}, {female,12}, {unknown,174}, {male,74}].

hhfuns:filter(fun({Gender,Age}) -> Gender == male andalso Age > 60 end, People).
% [{male,66}, {male,74}]







% Fold

%% find the maximum of a list
max([H|T]) -> max2(T, H).
max2([], Max) -> Max;
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).
 
%% find the minimum of a list
min([H|T]) -> min2(T,H).
min2([], Min) -> Min;
min2([H|T], Min) when H < Min -> min2(T,H);
min2([_|T], Min) -> min2(T, Min).
 
%% sum of all the elements of a list
sum(L) -> sum(L,0).
sum([], Sum) -> Sum;
sum([H|T], Sum) -> sum(T, H+Sum).




fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

[H|T] = [1,7,3,5,9,0,2,3].
hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T). % 9
hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T). % 0
hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)). % 21





reverse(L) -> fold(fun(X,Acc) -> [X|Acc] end, [], L).
 
map2(F,L) -> reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

filter2(Pred, L) ->
  F = fun(X,Acc) ->
    case Pred(X) of
      true  -> [X|Acc];
      false -> Acc
      end
  end,
reverse(fold(F, [], L)).




BIFs:
lists:map/2, lists:filter/2, lists:foldl/3, lists:foldr/3
all/2, any/2, dropwhile/2, takewhile/2, partition/2
flatten/1, flatlength/1, flatmap/2, merge/1, nth/2, nthtail/2, split/2 