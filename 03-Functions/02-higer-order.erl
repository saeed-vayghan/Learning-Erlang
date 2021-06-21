-module(functions).
-compile(export_all).

%% An important part of all functional programming languages is the ability to take a function you defined and then pass it as a parameter to another function.
%% A function that can accept other functions transported around that way is named a higher order function.

%% Funs encourages us to encapsulate common patterns of design into functional forms called higher order functions. These functions not only shortens programs,
%% but also produce clearer programs because the intended meaning of the program is explicitly rather than implicitly stated.

one() -> 1.
two() -> 2.
 
add(X, Y) -> X() + Y().

adder() ->
  ?MODULE:add(fun ?MODULE:one/0, fun ?MODULE:two/0).




double([H|T]) -> [2 * H | double(T)];
double([])    -> [].

add_one([H|T]) -> [H + 1 | add_one(T)];
add_one([])    -> [].

% These functions, double and add_one, have a very similar structure. We can exploit this fact and write a function map which expresses this similarity:
map(F, [H|T]) -> [F(H)|map(F, T)];
map(F, [])    -> [].

doublex(L)  -> map(fun(X) -> 2 * X end, L).
add_onex(L) -> map(fun(X) -> 1 + X end, L).


% map(F, List) is a function which takes a function F and a list L as arguments and returns the new list which is obtained by applying F to each of the elements in L.
% The process of abstracting out the common features of a number of different programs is called procedural abstraction.