-module(functions).
-compile(export_all).


%% Each function is built from a number of clauses.
%% The clauses are separated by semicolons ‘;’.
%% Each individual clause consists of a clause head, an optional guard and a body.
%% Arity is the number of arguments or operands taken by a function 


%%%%%%%%%%%%%%%%%%%%%%%%%%% Guard Sequences %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A guard sequence is a sequence of guards, separated by semicolon (;).
%% The guard sequence is true if at least one of the guards is true. (The remaining guards, if any, are not evaluated.)
%% Guard1;...;GuardK

%% A guard is a sequence of guard expressions, separated by comma (,). The guard is true if all guard expressions evaluate to true.
%% GuardExpr1,...,GuardExprN


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
convert({X, _}, Y) ->
  {cannot, convert, from, X, to, Y}.


%% Using apply
apply(Mod, Func, ArgList).
apply({Mod, Func}, ArgList).
apply({erlang, atom_to_list}, [abc]). % output: [97,98,99]


area({ circle, Radius })    -> 3.14159 * Radius * Radius;
area({ triangle, A, B, C }) -> S = (A + B + C) /2, math:sqrt(S * (S-A) * (S-B) * (S-C)).


fib(N) -> fib_iter(N, 0, 1).
fib_iter(0, Result, _Next) -> Result;
fib_iter(Iter, Result, Next) when Iter > 0 ->
  fib_iter(Iter-1, Next, Result + Next).


head([H|_]) -> H.
second([_, X|_]) -> X.


same(X, X) -> true;
same(_, _) -> false.


valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
  io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date, Y, M, D]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
  io:format("Stop feeding me wrong data!~n").


preschool()    -> 'Go to preschool'.
kindergarten() -> 'Go to kindergarten'.
grade_school() -> 'Go to grade school'.
what_grade(X)  ->
	if 
    X < 5  -> preschool();
    X == 5 -> kindergarten();
    X > 5  -> grade_school()
	end.


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