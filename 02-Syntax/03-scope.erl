-module(scopes).
-export([]).


f(X) ->
  case g(X) of
    true  -> A = h(X), B = T(X);
    false -> A = k(X)
  end,

  % more logic here

  h(A), % ok
  h(B), % Illegal

  io:format("End").