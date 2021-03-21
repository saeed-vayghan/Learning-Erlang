-module(event).
-compile(export_all).
-record(state, {server, name="", to_go=0}).


loop(S = #state{server=Server}) ->
  receive
    {Server, Ref, cancel} -> Server ! {Ref, ok}
  after S#state.to_go*1000 ->
    Server ! {done, S#state.name}
  end.

% 6> c(event).
% {ok,event}

% 7> rr(event, state).
% [state]

% 8> spawn(event, loop, [#state{server=self(), name="test", to_go=5}]).
% <0.60.0>

% 9> flush().
% ok

% 10> flush().
% Shell got {done,"test"}
% ok

% 11> Pid = spawn(event, loop, [#state{server=self(), name="test", to_go=500}]).
% <0.64.0>

% 12> ReplyRef = make_ref().
% #Ref<0.0.0.210>

% 13> Pid ! {self(), ReplyRef, cancel}.
% {<0.50.0>,#Ref<0.0.0.210>,cancel}

% 14> flush().
% Shell got {#Ref<0.0.0.210>,ok}
% ok



loopx(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} -> Server ! {Ref, ok}
  after T*1000 ->
    if
      Next =:= [] -> Server ! {done, S#state.name};
      Next =/= [] -> loop(S#state{to_go=Next})
    end
  end.


cancel(Pid) ->
  %% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
  end.


% 17> c(event).
% {ok,event}

% 18> f().
% ok

% 19> event:start("Event", 0).
% <0.103.0>

% 20> flush().
% Shell got {done,"Event"}
% ok

% 21> Pid = event:start("Event", 500).
% <0.106.0>

% 22> event:cancel(Pid).
% ok