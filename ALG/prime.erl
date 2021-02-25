-module(prime).
-compile(export_all).


range(N, N) ->
  [N];
range(Min, Max) ->
  [Min | range(Min+1, Max)].


remove_multiples(N, [H|T]) when H rem N == 0 ->
  remove_multiples(N, T);

remove_multiples(N, [H|T]) ->
  [H | remove_multiples(N, T)];

remove_multiples(_, []) ->
  [].


sieve([H|T]) ->
  [H | sieve(remove_multiples(H, T))];

sieve([]) ->
  [].


primes(Max) ->
  sieve(range(2, Max)).