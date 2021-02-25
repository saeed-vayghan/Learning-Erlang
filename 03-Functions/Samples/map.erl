-module(samples).
-export([map/2, filter/2]).


map(Func, [H|T]) ->
  [apply(Func, [H])| map(Func, T)];

map(_, []) ->
  [].



filter(Pred, [H|T]) ->
  case apply(Pred,[H]) of
    true ->
      [H|filter(Pred, T)];
    false ->
      filter(Pred, T)
  end;

filter(_, []) -> [].




% samples:map({math,factorial}, [1,2,3,4,5,6,7,8]).
% samples:filter({math,even}, [1,2,3,4,5,6,7,8,9,10]).