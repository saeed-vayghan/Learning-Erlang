-module(sc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Constants
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	% `init` function will be called as soon as `start_link` has been called.
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).


% The supervisor can be asked to start new child processes at any time, using a simplified form of the function supervisor:start_child/2.
% Each time sc_sup:start_child/2 is called, a new sc_element process is started, each with its own value and lease time,
% This results in a dynamically generated supervision tree.
% For other kinds of supervisors, if you want to add children dynamically, you must give a full child specification to start_child/2.
%
% With a simple_one_for_one supervisor, all children have the same specification, and the supervisor already knows it,
% so you only have to say “please start another one.” This is what you want to have here.
%
% When someone calls the start_child/2 API function, it results in a message being sent to the supervisor process,
% asking it to start a new child process using the start_link function in sc_element with the extra arguments Value and LeaseTime.
start_child(Value, LeaseTime) ->
	% it asks the running supervisor (identified by ?SERVER) to start a new child
	% passing it the extra arguments Value and LeaseTime.
	% You make this an API function to keep the implementation details encapsulated within this module.
	supervisor:start_child(?SERVER, [Value, LeaseTime]).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	% In this supervisor, the children are marked as temporary rather than permanent, meaning that if they die,
	% they should not be restarted, brutal_kill, indicating that the children should be terminated immediately when the supervisor shuts down

	% (For a simple_one_for_one supervisor, the supervisor won’t do anything to actively shut down the children,
	% instead, they’re expected to terminate when they receive the exit signal triggered by the death of the supervisor.
	% If the children are normal OTP behaviours, this is guaranteed. Specifying brutal_kill here is mostly to show intent.)

	Element  = {sc_element, {sc_element, start_link, []}, temporary, brutal_kill, worker, [sc_element]},
	Children = [Element],

	% A simple_one_for_one supervisor can start only one type of child, but can start any number of them;
	% all its children are dynamically added at runtime, and no child process is started when the supervisor starts up. 
	RestartStrategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.
