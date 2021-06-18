%% Each function is built from a number of clauses.
%% The clauses are separated by semicolons ‘;’.
%% Each individual clause consists of a clause head, an optional guard and a body.
%% Arity is the number of arguments or operands taken by a function 

right_age(X) when X >= 16, X =< 104 -> % The comma (,) acts in a similar manner to the operator andalso
  true;
right_age(_) ->
  false.  


wrong_age(X) when X < 16; X > 104 -> % The semicolon (;) acts like orelse 
  true;
wrong_age(_) ->
  false.


convert({fahrenheit, Temp}, celsius) ->
  {celsius, 5 * (Temp - 32) / 9};
convert({celsius, Temp}, fahrenheit) ->
  {farenheit, 32 + Temp * 9 / 5};
convert({reaumur, Temp}, celsius) ->
  {celsius, 10 * Temp / 8};
convert({celsius, Temp}, reaumur) ->
  {reaumur, 8 * Temp / 10};
convert({X, _}, Y) ->
  {cannot, convert, from, X, to, Y}.


%% Using apply
apply(Mod, Func, ArgList).
apply({Mod, Func}, ArgList).
apply({erlang, atom_to_list}, [abc]). % output: [97,98,99]


area({ square, Side }) ->
  Side * Side;
area({ rectangle, X, Y }) ->
  X * Y;
area({ circle, Radius }) ->
  3.14159 * Radius * Radius;
area({ triangle, A, B, C }) ->
  S = (A + B + C) /2,
  math:sqrt(S * (S-A) * (S-B) * (S-C)).


fib(N) -> fib_iter(N, 0, 1).
fib_iter(0, Result, _Next) -> Result;
fib_iter(Iter, Result, Next) when Iter > 0 ->
  fib_iter(Iter-1, Next, Result+Next).


%% A portion of guard tests is as follows:
%% is_atom(X)      %/ X is an atom
%% is_float(X)     %/ X is a float
%% is_integer(X)   %/ X is an integer
%% is_list(X)      %/ X is a list or []
%% is_number(X)    %/ X is an integer or float
%% is_pid(X)       %/ X is a process identifier
%% is_port(X)      %/ X is a port
%% is_reference(X) %/ X is a reference
%% is_tuple(X)     %/ X is a tuple
%% is_binary(X)    %/ X is a binary

%% In addition, certain BIFs, together with arithmetic expressions, are allowed in guards:
%% element/2, hd/1, length/1, round/1, self/0, size/1, trunc/1, tl/1, abs/1, node/1, node/0, nodes/0