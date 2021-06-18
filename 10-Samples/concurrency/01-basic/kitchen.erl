-module(kitchen).
-compile(export_all).


fridge1() ->
  receive
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge1();
    {From, {take, _Food}} ->
      %% uh....
      From ! {self(), not_found},
      fridge1();
    terminate ->
      ok
  end.

fridge2(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge2([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
            fridge2(lists:delete(Food, FoodList));
        false ->
          From ! {self(), not_found},
          fridge2(FoodList)
      end;
    terminate ->
      ok
  end.



% 1> c(kitchen).
% {ok,kitchen}

% 2> Pid = spawn(kitchen, fridge2, [[baking_soda]]).
% <0.51.0>

% 3> Pid ! {self(), {store, milk}}.
% {<0.33.0>,{store,milk}}

% 4> flush().
% Shell got {<0.51.0>,ok}
% ok

% 5> Pid ! {self(), {store, bacon}}.
% {<0.33.0>,{store,bacon}}

% 6> Pid ! {self(), {take, bacon}}.
% {<0.33.0>,{take,bacon}}

% 7> Pid ! {self(), {take, turkey}}.
% {<0.33.0>,{take,turkey}}

% 8> flush().
% Shell got {<0.51.0>,ok}
% Shell got {<0.51.0>,{ok,bacon}}
% Shell got {<0.51.0>,not_found}
% ok



start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.
