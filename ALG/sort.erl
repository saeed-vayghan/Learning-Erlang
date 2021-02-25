-module(sort).
-export([quicksort/1]).
-export([qsort/1]).


split(Pivot, L) ->
  split(Pivot, L, [], []).

split(Pivot, [], Smaller, Bigger) ->
  {Smaller, Bigger};

split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
  split(Pivot, T, [H|Smaller], Bigger);

split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
  split(Pivot, T, Smaller, [H|Bigger]).



quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  {Smaller, Bigger} = split(Pivot, Rest),
  lists:append(quicksort(Smaller), [Pivot|quicksort(Bigger)]).



%% qsort(A,B)
%% Inputs:
%% A = unsorted List
%% B = sorted list where all elements in B
%% are greater than any element in A
%% Returns
%% sort(A) appended to B

qsort(X) ->
  qsort(X, []).

qsort([Pivot|Rest], Tail) ->
  {Smaller,Bigger} = split(Pivot, Rest),
  qsort(Smaller, [Pivot|qsort(Bigger,Tail)]);

qsort([], Tail) ->
  Tail.