-module(conditions).
-export([]).


%%% Term comparisons %%%

%% coerce
% X > Y
% X < Y
% X =< Y
% X >= Y
% X == Y (X equal to Y)
% X /= Y (X not equal to Y)

%% exact
% X =:= Y (X equal to Y)
% X =/= Y (X not equal to Y)

% Thus 5.0 == 1 + 4 succeeds whereas 5.0 =:= 1 + 4 fails.


start() -> 
   X = 40, 
   Y = 50, 
   
   Res1 = X + Y, 
   Res2 = X - Y, 
   Res3 = X * Y, 
   Res4 = X / Y, % Division of numerator by denominator
   Res5 = X div Y, % The div component will perform the division and return the integer component.
   Res6 = X rem Y. % Remainder of dividing the first number by the second




%%% case of %%%
case Expr of
  Pattern1 [when Guard1] -> Seq1;
  Pattern2 [when Guard2] -> Seq2;
  ...
  PatternN [when GuardN] -> SeqN
end

case allocate(Resource) of
  {yes, Address} when Address > 0, Address =< Max ->
    Sequence 1 ... ;

  no ->
    Sequence 2 ...;

  _ ->
    true
end


insert(X, [])  -> [X];
insert(X, Set) ->
  case lists:member(X, Set) of
    true  -> Set;
    false -> [X|Set]
  end.


beach(Temperature) ->
  case Temperature of
    {celsius, N}    when N >= 20,  N =< 45  -> 'favorable';
    {kelvin, N}     when N >= 293, N =< 318 -> 'scientifically favorable';
    {fahrenheit, N} when N >= 68,  N =< 93  -> 'favorable in the US';
    _ -> 'avoid beach'
  end.




%%% if %%%
if
  Guard1 ->
    Sequence1 ;

  Guard2 ->
    Sequence2 ;

  true -> % catchall if necessary
    Sequence3 ;
end



help_me(Animal) ->
  Talk = if 
    Animal == cat  -> "meow";
    Animal == beef -> "mooo";
    Animal == dog  -> "bark";
    Animal == tree -> "bark";
    true -> "fgdadfgna"
  end,
  {Animal, "says " ++ Talk ++ "!"}.