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



factorial_1(0) -> 1;
factorial_1(N) -> N * factorial_1(N - 1).


factorial_2(0) -> 1;
factorial_2(N) when N > 0 -> N * factorial_2(N - 1).


factorial_3(N) ->
  if
    N == 0 -> 1;
    N >  0 -> N * factorial_3(N - 1)
  end.


factorial_4(N) ->
  case N of
    0 -> 1;

    N when (N > 0) -> N * factorial_4(N - 1)
  end.


factorial_efficient(N)     -> factorial_efficient(N, 1).
factorial_efficient(0, ACC) -> ACC;
factorial_efficient(N, ACC) -> factorial_efficient(N - 1, ACC * N).
  


allocate(Resource) when Resource == 'memory' -> {yes, 64};
allocate(_) -> 'no'.

my_func(Resource) ->
  Max = 128,

  case allocate(Resource) of
    {yes, Address} when Address > 0, Address =< Max -> 'succeeds';
    no -> 'fails'
  end.


% allocate:my_func('memory').