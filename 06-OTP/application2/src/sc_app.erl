-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % it’s a good design principle to limit the amount of application code in supervisors in order to keep them small and reliable.
    % so we put `sc_store:init()` inside the application part 
    sc_store:init(), % initializing storage
    case sc_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
