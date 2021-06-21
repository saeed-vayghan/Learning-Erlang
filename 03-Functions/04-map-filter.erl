-module(functions).
-compile(export_all).

% https://erlang.org/doc/man/lists.html


%% Using lists:map
Adder = fun(X) -> X + 3 end.
lists:map(Adder, [1, 2, 3]). % [4,5,6]

incr(X) -> X + 1.
lists:map(fun functions:incr/1, L).  % [2,3,4,5,6]


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



% BIFs:
% lists:map/2, lists:filter/2, lists:foldl/3, lists:foldr/3
% all/2, any/2, dropwhile/2, takewhile/2, partition/2
% flatten/1, flatlength/1, flatmap/2, merge/1, nth/2, nthtail/2, split/2 