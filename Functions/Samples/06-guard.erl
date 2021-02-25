

% Guard Sequences


% A guard sequence is a sequence of guards, separated by semicolon (;).
% The guard sequence is true if at least one of the guards is true. (The remaining guards, if any, are not evaluated.)
% Guard1;...;GuardK



% A guard is a sequence of guard expressions, separated by comma (,). The guard is true if all guard expressions evaluate to true.
% GuardExpr1,...,GuardExprN





-module(guards).
-compile([export_all]).


age(Age) when Age > 19 ->
  adult;

age(Age) when Age >= 13, Age =< 19 ->
  teen;

age(Age) when Age >= 3, Age < 13 ->
  child;

age(Age) when Age >= 1, Age < 3 ->
  toddler.



right_age(X) when X >= 16, X =< 104 ->
  true;

right_age(_) ->
  false.


wrong_age(X) when X < 16; X > 104 ->
  true;

wrong_age(_) ->
  false.




% Ifs act like guards and share guards' syntax, but outside of a function clause's head. In fact, the if clauses are called Guard Patterns.

heh_fine() ->
  if 1 =:= 1 ->
    works
  end,

  if 1 =:= 2; 1 =:= 1 ->
    works
  end,

  if 1 =:= 2, 1 =:= 1 ->
    fails
  end.