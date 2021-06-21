-module(tr_app).

% behaviour declaration
-behaviour(application).

% callbacks of application behaviour
-export([start/2, stop/1]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------

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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------

% The stop/1 callback is simple in this case: you don’t need to do anything special on shutdown, so you ignore the input parameter
stop(_State) ->
  ok.
