-module(naming).
-compile([export_all]).


start_critic() ->
  spawn(?MODULE, critic, []).

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





% 1> c(naming).
% {ok,naming}
% 2> Critic = naming:start_critic().
% <0.47.0>
% 3> naming:judge(Critic, "Genesis", "The Lambda Lies Down on Broadway").
% "They are terrible!"
% 4> exit(Critic, solar_storm).
% true
% 5> naming:judge(Critic, "Genesis", "A trick of the Tail Recursion").
% timeout