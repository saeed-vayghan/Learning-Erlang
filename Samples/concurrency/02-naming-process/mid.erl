-module(naming).
-compile([export_all]).


start_critic() ->
  spawn(?MODULE, restarter, []).

% The problem with our approach is that there is no way to find the Pid of the critic, and thus we can't call him to have his opinion.
% One of the solutions Erlang has to solve this is to give names to processes.
restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  receive
    {'EXIT', Pid, normal}   -> ok; % not a crash        
    {'EXIT', Pid, shutdown} -> ok; % manual shutdown, not a crash
    {'EXIT', Pid, _} -> restarter()
  end.


judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {"System of a Downtime", "Memoize"}} ->
      From ! {self(), "They're not Johnny Crash but they're good."};
    {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {self(), "Simply incredible."};
    {From, {_Band, _Album}} ->
      From ! {self(), "They are terrible!"}
  end,
  critic().
