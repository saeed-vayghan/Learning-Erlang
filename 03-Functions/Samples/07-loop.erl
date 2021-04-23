foreach(Fun, [First|Rest]) ->
    Fun(First),
    foreach(Fun, Rest);
foreach(Fun, []) ->
    ok.

map(Fun, [First|Rest]) -> 
    [Fun(First)|map(Fun,Rest)];
map(Fun, []) -> 
    [].



Add_3 = fun(X) -> X + 3 end.
#Fun<erl_eval.5.123085357>

lists:map(Add_3, [1,2,3]).
[4,5,6]



convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    New_list = lists:map(fun convert_to_c/1, List),
    Sorter = fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end.
    lists:sort(Sorter, New_list).
