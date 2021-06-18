-module(tr_app).

% behaviour declaration
-behaviour(application).

% callbacks of application behaviour
-export([
     start/2,
     stop/1
     ]).


% {mod, {tr_app, []}} in .pp file
% _Type would be one of `normal`, `{failover, ...}`, `{takeover, ...}`
start(_Type, _StartArgs) ->
    % starts root supervisor
    % The only real job to be done by the فق_app module is to start the root supervisor when the application is started  
    case tr_sup:start_link() of
      {ok, Pid} ->
          {ok, Pid};
      Other ->
          {error, Other}
    end.

% The stop/1 callback is simple in this case: you don’t need to do anything special on shutdown, so you ignore the input parameter
stop(_State) ->
    ok.
