
% Each function is built from a number of clauses.
% The clauses are separated by semicolons ‘;’.
% Each individual clause consists of a clause head, an optional guard and a body.
% Arity is the number of arguments or operands taken by a function 

factorial(N) when N == 0 -> % (N == 0) gaurd
  1; % body

factorial(N) when N > 0 ->
  N1 = N - 1,
  F1 = factorial(N1),
  N * F1.

right_age(X) when X >= 16, X =< 104 -> % The comma (,) acts in a similar manner to the operator andalso
  true;
right_age(_) ->
  false.  

wrong_age(X) when X < 16; X > 104 -> % The semicolon (;) acts like orelse 
  true;
wrong_age(_) ->
  false.


% A portion of guard tests is as follows:

% is_atom(X)      %/ X is an atom
% is_float(X)     %/ X is a float
% is_integer(X)   %/ X is an integer
% is_list(X)      %/ X is a list or []
% is_number(X)    %/ X is an integer or float
% is_pid(X)       %/ X is a process identifier
% is_port(X)      %/ X is a port
% is_reference(X) %/ X is a reference
% is_tuple(X)     %/ X is a tuple
% is_binary(X)    %/ X is a binary

% In addition, certain BIFs, together with arithmetic expressions, are allowed in guards:
%% element/2, hd/1, length/1, round/1, self/0, size/1, trunc/1, tl/1, abs/1, node/1, node/0, nodes/0


%% Examples of guarded function clause heads:

% foo(X) when list(X), hd(X) == head, hd(tl(X)) == head_2 -> 'do-sth';
% foo(X, Y, Z) when is_integer(X), is_integer(Y), is_integer(Z), X == Y + Z -> 'do-sth';
% foo(X, Y, Z) when list(X), hd(X) == {Y, length(Z)} -> 'do-sth';
% foo(X, Y, Z) when {X, Y, size(Z)} == {a, b, 5} -> 'do-sth'.



%%%  Using apply %%%

% > apply(Mod, Func, ArgList).
% > apply({Mod, Func}, ArgList).

% > apply({erlang, atom_to_list}, [abc]).
% [97,98,99]
