-module(my_math).

-export([factorial/1]).
-export([double/1]).
-export([area/1]).


factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

times(M, N) ->  M * N.
double(X) -> times(X, 2).


area({ square, Side }) ->
  Side * Side;

area({ rectangle, X, Y }) ->
  X * Y;

area({ circle, Radius }) ->
  3.14159 * Radius * Radius;

area({ triangle, A, B, C }) ->
  S = (A + B + C) /2,
  math:sqrt(S * (S-A) * (S-B) * (S-C)).



% my_math:factorial(10).
% my_math:double(5).

% my_math:double(my_math:double(2)).

% my_math:area({triangle, 3, 4, 5}).
% my_math:area({square, 5}).

% Thing = {circle, 5}. ==> tuple {atom, int}
% my_math:area(Thing).