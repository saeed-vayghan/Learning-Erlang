-module(lists).
-compile(export_all).


% It is important to remember that tuples should always be used when dealing with a fixed number of items.
% Tuples use approximately half the storage of lists and have much faster access.
% Lists should be used when the problem needs a variable number of items.


%%%%%%%%%%%%%%%% Simple List %%%%%%%%%%%%%%%%
List = [2, 3,4]. % [2,3,4]
NewList = [1 | List]. % [1,2,3,4]
% The | we used is named the cons operator (constructor).


%%%%%%%% Built-in functions %%%%%%%%
atom_to_list(hello) % ==> [104,101,108,108,111].
float_to_list(1.5) % ==> [49,46,53,48,48,...,48].
integer_to_list(1245) % ==> [49,50,52,53].
list_to_atom([119,111,114,108,100]) % ==> world.
list_to_float([51,46,49,52,49,53,57]) % ==> 3.14159.
list_to_integer([49,50,51,52]) % ==> 1234.
hd([a, b, c, d]) % ==> a.
tl([a, b, c, d]) % ==> [b,c,d].
length([a, b, c, d]) % ==> 4.

Pid = list_to_pid(AsciiList)
pid_to_list(Pid)

lists:member(X, L) % returns true if X is an element of the list L, otherwise false.
my_member(X, [X|_]) -> true;
my_member(X, [_|T]) -> my_member(X, T);
my_member(X, []) -> false.

lists:append(A, B) % concatenates the two lists A and B.
my_append([H|T], B) -> [H|my_append(T, B)];
my_append([], B) -> B.

lists:reverse(L)
my_reverse(L) -> my_reverse(L, []).
my_reverse([H|T], Acc) -> my_reverse(T, [H|Acc]);
my_reverse([], Acc) -> Acc.

lists:delete_all(X, L) % deletes all occurrences of X from the list L.
lists:seq(1,4). % [1,2,3,4]


List1 = [1,2,3].
List2 = [4,5,6].

% Join lists with ++
List3 = List1 ++ List2.

% Subtract a list
List4 = List3 -- List1.


%%%%%%%%%%%%%%%% Fold %%%%%%%%%%%%%%%%

P = fun(A, AccIn) ->
  % io:format("~p * ~p \n", [A, AccIn]),
  A * AccIn
end.

lists:foldl(P, 5, [1,2,3]).
% 1 * 5 <== new AccIn becomes 5
% 2 * (1 * 5) <== new AccIn becomes 10
% 3 * (2 * (1 * 5)


P = fun(A, Acc) ->
  io:format("[~p | ~p] \n", [A, Acc]),
  [A | Acc]
end.

lists:foldl(P, [], [1,2,3]).
% [1 | []] <== new Acc becomes [1]
% [2 | [1]]  <== new Acc becomes [2, 1]
% [3 | [2,1]] 
% [3,2,1]
