% https://erlang.org/doc/man/maps.html

% #{ K1 => V1, .., Kn => Vn }
% M0 = #{},                 % empty map
% M1 = #{a => <<"hello">>}, % single association with literals
% M2 = #{1 => 2, b => b},   % multiple associations with literals
% M3 = #{k => {A,B}},       % single association with variables
% M4 = #{{"w", 1} => f()}.  % compound key associated with an evaluated expression



% Map helpers, It is just a simple sampel to filter out some items.
MyMap = #{ K1 => V1, .., Kn => Vn }
Range  = [K5, K7, K9],
Updated = maps:without(Range, MyMap)  



%% One way to extract data fram a map
Transaction = #{key => my_key, bank => my_bank}.

BankName = case Transaction of
  #{bank := Bank} -> Bank;
  _ -> undefined
end.