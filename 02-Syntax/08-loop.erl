
% You can create a for loop with recursion
for(0, _) -> 
  ok; 
  
for(Max, Min) when Max > 0 -> 
  io:fwrite("Num : ~p\n", [Max]), 
  for(Max - 1, Min). 


