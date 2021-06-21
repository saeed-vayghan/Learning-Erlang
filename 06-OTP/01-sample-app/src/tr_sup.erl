-module(tr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
  % starts supervisor

  % ?MODULE passing the module name to the supervisor

  % {local, ?SERVER} in the first argument to the call tells the library to automatically register the 
  % supervisor process on the local node under the name tr_sup 

  % [] is passed to the init/1 callback function on startup
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  % says how to start and manage childred

  % child `{ID,Start,Restart,Shutdown,Type,Modules}`
  % ID, is a term that the supervisor uses to identify the module internally

  % Start, is a triple {Module, Function, Arguments} that is used to start the process.
  
  % Restart, says whether this is a child that should be restarted upon failure. 
  %% permanent: should always be restarted
  %% temporary: should never be restarted
  %% transient: should be restarted only if they terminate abnormally but not upon normal termination.
  
  % Shutdown, says how the process may be killed
  %% milliseconds
  %% brutal_kill, meaning that the process will always be terminated immediately
  %% infinity, which is used mainly when the child itself is a supervisor and should be given all the time it needs.

  % Type, indicates whether the process is a supervisor or a worker.
  % The sixth item lists the modules that this process depends on 

  Server = {tr_server, {tr_server, start_link, []}, permanent, 2000, worker, [tr_server]},
  Children = [Server],

  % how supervisor should behave , format is `{How, Max, Within}`
  RestartStrategy = {one_for_one, 0, 1},

  % format of the return `{ok, {RestartStrategy, Children}}`
  {ok, {RestartStrategy, Children}}.
