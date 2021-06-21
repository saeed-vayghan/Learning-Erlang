-module(sc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

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
start(_StartType, _StartArgs) ->
  % Starts root supervisor
  % The only real job to be done by the sc_app module is to start the root supervisor when the application is started
  %
  % It’s a good design principle to limit the amount of application code in supervisors in order to keep them small and reliable.
  % so we put `sc_store:init()` inside the application part 
  sc_store:init(), % initializing storage

  case sc_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Other     -> {error, Other}
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

stop(_State) ->
  ok.
