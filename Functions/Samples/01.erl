-module(functions).
-export([head/1, second/1, same/2, valid_time/1]).


head([H|_]) -> H.

second([_, X|_]) -> X.

same(X, X) ->
    true;
same(_, _) ->
    false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date, Y, M, D]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").



% > functions:valid_time({{2011,09,06},{09,04,43}}).
% The Date tuple ({2011,9,6}) says today is: 2011/9/6,
% The time tuple ({9,4,43}) indicates: 9:4:43.
% ok

% 6> functions:valid_time({{2011,09,06},{09,04}}).
% Stop feeding me wrong data!
% ok



% In Erlang the last expression in your function is returned.
% To return Cmd you can simply make it the last expression in your function:
% function_test() ->
%     Cmd = os:cmd("ls"),
%     io:format("The result of ls is:~p~n", [Cmd]),
%     Cmd.
