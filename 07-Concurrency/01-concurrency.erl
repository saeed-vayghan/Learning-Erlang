-module(kitchen).
-compile(export_all).


% ?MODULE is a macro returning the current module's name

% receive
%   Pattern1 when Guard1 -> Expr1;
%   Pattern2 when Guard2 -> Expr2;
%   Pattern3 -> Expr3
% end

% receive
%   Match -> Expression1
% after Delay ->
%   Expression2
% end.




start(FoodList) ->
  spawn(?MODULE, fridge2, [FoodList]).

store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  end.

store2(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

take2(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

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




% c(kitchen).
% {ok,kitchen}

% Pid = kitchen:start([rhubarb, dog, hotdog]).
% <0.84.0>

% kitchen:take(Pid, dog).
% {ok,dog}

% kitchen:take(Pid, dog).
% not_found
