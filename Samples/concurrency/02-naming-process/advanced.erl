-module(naming).
-compile([export_all]).


start_critic() ->
  spawn(?MODULE, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),

  % The act of giving a name to a process allows you to replace the unpredictable pid by an atom.
  % If the process dies, it will automatically lose its name or you can also use unregister/1 to do it manually.
  register(critic, Pid),

  receive
    {'EXIT', Pid, normal}   -> ok; % not a crash        
    {'EXIT', Pid, shutdown} -> ok; % manual shutdown, not a crash
    {'EXIT', Pid, _} -> restarter()
  end.


judge(Band, Album) ->
  critic ! {self(), {Band, Album}},

  % Here, the line Pid = whereis(critic) is used to find the critic's process identifier.
  Pid = whereis(critic),
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

judge2(Band, Album) ->
  % created with make_ref() as unique values to identify messages
  Ref = make_ref(),

  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {Ref, "They are great!"};
    {From, Ref, {"System of a Downtime", "Memoize"}} ->
      From ! {Ref, "They're not Johnny Crash but they're good."};
    {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {Ref, "Simply incredible."};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "They are terrible!"}
  end,
  critic().



% 6> c(naming).
% {ok,naming}
% 7> naming:start_critic().
% <0.55.0>
% 8> naming:judge2("The Doors", "Light my Firewall").
% "They are terrible!"
% 9> exit(whereis(critic), kill).
% true
% 10> naming:judge2("Rage Against the Turing Machine", "Unit Testify").
% "They are great!"
