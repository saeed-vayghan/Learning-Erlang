-module(prime).
-compile(export_all).



[1,2,3] ++ [4,5]. % [1,2,3,4,5]
[1,2,3,4,5] -- [1,2,3]. % [4,5]
[2,4,2] -- [2,4]. % [2]
[2,4,2] -- [2,4,2]. % []

% Both ++ and -- are right-associative.
[1,2,3] -- [1,2] -- [3]. % [3]
[1,2,3] -- [1,2] -- [2]. % [2,3]



% search(X, [X|T]) ->
%   ... do something ...
%   ...;

% search(X, [_|T]) ->
%   search(X, T);

% search(X, []) ->
%   ... didn’t find it ...


sum([]) -> 0;
sum([H|T]) -> H + sum(T).

% Tail Recursive
sum2([], Sum) -> Sum;
sum2([H|T], Sum) -> sum2(T, H + Sum).




isomorphic([X|T]) -> [something(X) | isomorphic(T)];
isomorphic([]) -> [].


double([H|T]) -> [2 * H | double(T)];
double([]) -> [].


doublex([H|T]) when is_integer(H)-> [2 * H | doublex(T)];
doublex([H|T]) when is_list(H) -> [doublex(H) | doublex(T)];
doublex([]) -> [].

%% lists1:doublex([1,2,[3,4],[5,[6,12],3]]).
%% [2,4,[6,8],[10,[12,24],6]]


last_tail([_|T]) -> last_tail(T);
last_tail(Tail) -> Tail

nth(1, [H|T]) -> H;
nth(N, [_|T]) -> nth(N - 1, T).


nth1(N, L) -> nth1(1, N, L).
nth1(Max, Max, [H|_]) -> H;
nth1(N, Max, [_|T]) -> nth1(N+1, Max, T).


%%%
%%% Use of accumulators is often preferable to building the result in the recursion itself.
%%% This leads to flat code which executes in constant space.

% collect(L) -> collect(L, []).
%
% collect([H|T], Accumulator) ->
%   case pred(H) of
%     true ->
%       collect(T, [dosomething(H) | Accumulator]);
%     false ->
%       collect(T, Accumulator)
%   end;
%
% collect([], Accumulator) -> Accumulator.





even(X) when X >= 0 -> (X band 1) == 0.
odd(X)  when X > 0  -> not even(X).


funny(L) -> funny(L, []).

funny([H|T], Accumulator) ->
  case even(H) of
    true  -> funny(T, [H*H | Accumulator]);
    false -> funny(T, Accumulator)
  end;

funny([], Accumulator) -> Accumulator.

%% > lists:funny([1,2,3,4,5,6])
%% [36,16,4]





% NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN].
% The part "Pattern <- List" is named a Generator expression. You can have more than one!

[X+Y || X <- [1,2], Y <- [2,3]].
% [3,4,4,5]


%% The arrow acts exactly like the = operator, with the exception that it doesn't throw exceptions.
[2*N || N <- [1,2,3,4]].
% [2,4,6,8]


[X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
% [2,4,6,8,10]


RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}].
[{Item, Price*1.07} || {Item, Price} <- RestaurantMenu, Price >= 3, Price =< 10].
% [{steak, 6.409300000000001}, {beer, 4.2693}, {poutine,3.745}]


Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
FoggyPlaces = [X || {X, fog} <- Weather].
% [london, boston]
%% If an element of the list 'Weather' doesn't match the {X, fog} pattern,
%% it's simply ignored in the list comprehension whereas the "=" operator would have thrown an exception.