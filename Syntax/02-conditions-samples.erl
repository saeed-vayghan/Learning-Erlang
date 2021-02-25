-module(conditions).
-export([]).


even(X) when X >= 0 -> (X band 1) == 0.
odd(X)  when X > 0  -> not even(X).



start(A, B) -> 
  if
    A == B -> 
      io:fwrite("A == B");

    A > B ->
      io:fwrite("A > B");

    A < B ->
      if
        A > 5 ->
          io:fwrite("A < B and A is greater than 5");
        true ->
          io:fwrite("A < B and A is less than 5")
      end;

    true ->
      io:fwrite("False")
  end.

start(A) -> 
  case A of 
    5 -> io:fwrite("The value of A is 5");
    6 -> io:fwrite("The value of A is 6")
  end.



factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

factorial(N) ->
  if
    N == 0 -> 1;
    N >  0 -> N * factorial(N - 1)
  end.

factorial(N) ->
  case N of
    0 -> 1;

    N when (N > 0) -> N * factorial(N - 1)
  end.



allocate(Resource) when Resource == 'memory' -> {yes, 64};
allocate(_) -> 'no'.

my_func(Resource) ->
  Max = 128,

  case allocate(Resource) of
    {yes, Address} when Address > 0, Address =< Max -> 'succeeds';
    no -> 'fails'
  end.


% allocate:my_func('memory').