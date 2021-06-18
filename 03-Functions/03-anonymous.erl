
fun
  (Args1) -> Expression1, Exp2, ..., ExpN;
  (Args2) -> Expression1, Exp2, ..., ExpN;
  (Args3) -> Expression1, Exp2, ..., ExpN
end



% Clousure
PrepareAlarm = fun(Room) ->
  io:format("Alarm set in ~s.~n",[Room]),
  fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
end.



a() ->
  Secret = "pony",
  fun() -> Secret end.
 
b(F) -> "Password is " ++ F().
b(a()). % "Password is pony"



PrepareAlarm = fun(Room) ->
  io:format("Alarm set in ~s.~n", [Room]),

  fun Loop() ->
    io:format("Alarm tripped in ~s! Call Batman!~n", [Room]),
    timer:sleep(500),
    Loop()
  end
end.


% Alarm set in bathroom.
AlarmReady = PrepareAlarm("bathroom"). 
AlarmReady().
% Alarm tripped in bathroom! Call Batman!
% Alarm tripped in bathroom! Call Batman!
% Alarm tripped in bathroom! Call Batman!
% ...
