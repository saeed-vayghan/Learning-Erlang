-module(multiproc).
-compile([export_all]).


% In this specific case, no message will ever be matched in the receive part of the construct because there is no pattern.
% Instead, the after part of the construct will be called once the delay T has passed.
sleep(T) ->
  receive
  after T -> ok
  end.

% In this case, any message matches. As long as there are messages, the flush/0 function will recursively call itself until the mailbox is empty.
% Once this is done, the after 0 -> ok part of the code is executed and the function returns.
flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.




important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
    normal()
  end.

normal() ->
  receive
    {_, Message} -> [Message | normal()]
  after 0 ->
    []
  end.

%% optimized in R14A
optimized(Pid) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, hello},
  receive
    {Pid, Ref, Msg} -> io:format("~p~n", [Msg])
  end.



self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}. 