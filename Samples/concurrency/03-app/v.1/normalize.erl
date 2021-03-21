

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].



Normx = fun(N) ->
  Limit = 49,
  [N rem Limit | lists:duplicate(N div Limit, Limit)]
end.



% 27> Normx(102).                                   
% [4,49,49]

% 28> Normx(110). 
% "\f11"