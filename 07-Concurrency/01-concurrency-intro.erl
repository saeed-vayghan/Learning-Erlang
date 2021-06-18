% ! <== bang symbol


F = fun() -> 2 + 2 end.
spawn(F).
% <0.44.0> <== PID (process identifier)
% We only get its pid. That's because processes do not return anything.

spawn(fun() -> io:format("~p~n",[2 + 2]) end).
% 4



G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end.
[spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].



self() ! hello.
% hello <== The return value of the send operation

self() ! self() ! double. <===> self() ! (self() ! double).




% Due to the selective receives, there is possibility of growing the queue size and making your application running slow.
% If such selective receives are effectively causing a massive slowdown in your code, the first thing to do is to ask yourself is:
%% Why you are getting messages you do not want.
%% Are the messages sent to the right processes?
%% Are the patterns correct?
%% Are the messages formatted incorrectly?
%% Are you using one process where there should be many?

% Erlang programmers sometimes take a defensive measure against such events. A standard way to do it might look like this:
% The Unexpected variable will match anything, take the unexpected message out of the mailbox and show a warning

% receive
%   Pattern1 -> Expression1;
%   Pattern2 -> Expression2;
%   Pattern3 -> Expression3;
%   ...
%   PatternN -> ExpressionN;
%   Unexpected ->
%     io:format("unexpected message ~p~n", [Unexpected])
% end.





%%%%%%%%%%%%%%%%%%%%% Link %%%%%%%%%%%%%%%%%%%%%
% A link is a specific kind of relationship that can be created between two processes.
% When that relationship is set up and one of the processes dies from an unexpected throw, error or exit (see Errors and Exceptions), the other linked process also dies.

% links aren't stackable, so the moment you unlink one, you unlink them all and mess up all the assumptions put up by the other libraries. 

myproc() ->
  timer:sleep(5000),
  exit(reason).

% spawn(fun linkmon:myproc/0).
% link(spawn(fun linkmon:myproc/0)).







%%%%%%%%%%%%%%%%%%%%% Error %%%%%%%%%%%%%%%%%%%%%

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;

chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  receive
    _ -> ok
  end.

% Error propagation across processes is done through a process similar to message passing, but with a special type of message called signals.
% Exit signals are 'secret' messages that automatically act on processes, killing them in the action.
% link(spawn(linkmon, chain, [3])).
% link(spawn(?MODULE, chain, [N-1]))



% In order to be reliable, an application needs to be able to both kill and restart a process quickly. 
  % Links are alright to do the killing part.

  % In order to restart a process, we need a way to first know that it died. This can be done by adding a layer on top of links with a concept called system processes.
  % System processes are basically normal processes, except they can convert exit signals to regular messages.
  % This is done by calling process_flag(trap_exit, true) in a running process.





%%%%%%%%%%%%%%%%%%%%% Monitors %%%%%%%%%%%%%%%%%%%%%

% monitors are a special type of link with two differences:
%   they are unidirectional
%   they can be stacked

% Monitors are what you want when a process wants to know what's going on with a second process, but neither of them really are vital to each other.
% They can be removed individually. Plus, being unidirectional is handy in libraries because other processes shouldn't have to be aware of said libraries.

erlang:monitor(process, spawn(fun() -> timer:sleep(500) end)). % response: #Ref<0.0.0.77>
flush().
% Shell got {'DOWN',#Ref<0.3371885426.106168324.158055>,process,<0.81.0>,normal} <== This is the structure of received message in the mailbox
% Every time a process you monitor goes down, you will receive such a message. The message is {'DOWN', MonitorReference, process, Pid, Reason}.
% The reference is there to allow you to demonitor the process. Remember, monitors are stackable, so it's possible to take more than one down.
% References allow you to track each of them in a unique manner. 


{Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end). % response: {<0.92.0>,#Ref<0.3371885426.106168324.158100>}

Pid ! die.
Is_monitor_exsisted = erlang:demonitor(Ref, [flush, info]).
% True or False

% The info option tells you if a monitor existed or not when you tried to remove it. 
% This is why the expression 10 returned false. Using flush as an option will remove the DOWN message from the mailbox if it existed.

% erlang:demonitor(Ref).
% flush().





%%%%%%%%%%%%%%%%%%%%% Monitors %%%%%%%%%%%%%%%%%%%%%



