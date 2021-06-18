loop(Map) when is_map(Map) -> 
  Keys = maps:keys(Map),
  loop(Map, Keys).

loop(_ , []) ->
  ok;
loop(Map, [Head|Tail]) ->
  Value = maps:get(Head, Map),
  io:format("~p: ~p~n", [Head, Value]),
  loop(Map, Tail).


% Map = #{1 => "one", 2 => "two", 3 => "three"}.
% loop(Map).
% % will return:
% % 1: "one"
% % 2: "two"
% % 3: "three"




Transaction = #{key => my_key, bank => my_bank}.

BankName = case Transaction of
  #{bank := Bank} -> Bank;
  _ -> undefined
end,

BankName.