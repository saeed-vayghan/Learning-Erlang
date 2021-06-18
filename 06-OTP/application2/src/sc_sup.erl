-module(sc_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


% it asks the running supervisor (identified by ?SERVER) to start a new child,
% passing it the extra arguments Value and LeaseTime.
% You make this an API function to keep the implementation details encapsulated within this module.

% Each time sc_sup:start_child/2 is called, a new sc_element process is started, each with its own value and lease time.
% This results in a dynamically generated supervi- sion tree
start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

init([]) ->

    % In this supervisor, the children are marked as temporary rather than permanent, meaning that if they die, they should not be restarted
    % brutal_kill, indicating that the children should be terminated immediately when the supervisor shuts down

    % (For a simple_one_for_one supervisor, the supervisor won’t do anything to actively shut down the children;
    % instead, they’re expected to terminate when they receive the exit signal triggered by the death of the supervisor.
    % If the children are normal OTP behaviours, this is guaranteed. Specifying brutal_kill here is mostly to show intent.)

    Element = {sc_element, {sc_element, start_link, []}, temporary, brutal_kill, worker, [sc_element]},
    Children = [Element],

    % A simple_one_for_one supervisor can start only one type of child, but can start any number of them;
    % all its children are dynamically added at runtime, and no child process is started when the supervisor starts up. 
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
