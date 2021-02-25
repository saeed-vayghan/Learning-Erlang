-module(sets).
-export([new/0, add_element/2, del_element/2, is_element/2, is_empty/1, union/2, intersection/2]).

new() -> [].

add_element(X, Set) ->
  case is_element(X, Set) of
    true -> Set;
    false -> [X|Set]
  end.

del_element(X, [X|T]) -> T;
del_element(X, [Y|T]) -> [Y|del_element(X,T)];
del_element(_, []) -> [].

is_element(H, [H|_]) -> true;
is_element(H, [_|Set]) -> is_element(H, Set);
is_element(_, []) -> false.

is_empty([]) -> true;
is_empty(_) -> false.

union([H|T], Set) -> union(T, add_element(H, Set));
union([], Set) -> Set.

intersection(S1, S2) -> intersection(S1, S2, []).
intersection([], _, S) -> S;
intersection([H|T], S1, S) ->
  case is_element(H,S1) of
    true -> intersection(T, S1, [H|S]);
    false -> intersection(T, S1, S)
  end.







% > S1 = sets:new().
% []
% > S2 = sets:add_element(a, S1).
% [a]
% > S3 = sets:add_element(b, S2).
% [b,a]
% > sets:is_element(a, S3).
% true
% > sets:is_element(1, S2).
% false
% > T1 = sets:new().
% []
% > T2 = sets:add_element(a, T1).
% [a]
% > T3 = sets:add_element(x, T2).
% [x,a]
% > sets:intersection(S3, T3).
% [a]
% 10> sets:union(S3,T3).
% [b,x,a]