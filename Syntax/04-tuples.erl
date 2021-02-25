-module(tuples).
-export([]).



tuple_to_list(T)
% Example: tuple_to_list({1,2,3,4}) ⇒ [1,2,3,4].

list_to_tuple(L)
% Example: list_to_tuple([a,b,c]) ⇒ {a,b,c}.

element(N, T)
% Example: element(3,{a,b,c,d}) ⇒ c.

setelement(N, T, Val)
% Example: setelement(3, {a,b,c,d}, xx) ⇒ {a,b,xx,d}.

size(T)
% Example: size({a,b,c}) ⇒ 3.