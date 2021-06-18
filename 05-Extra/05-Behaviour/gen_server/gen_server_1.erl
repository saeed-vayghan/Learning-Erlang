% Behaviour is a way of formatting common patterns in process-oriented programming.
% gen_server OTP behaviour takse server-client pattern and devides it into two halves, generic part and app-specific implementation part.

-module(gen_server_1).
-behaviour(gen_server).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SLEEP_TIME, (2*1000)).


% More: https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
% More: http://20bits.com/article/erlang-a-generic-server-tutorial

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    io:format("start_link: ~p~n", [Return]),
    Return.

% third part of the return statement of the init function,
% server timeout functionality B, letting the init/1 function specify a timeout limit in milliseconds.
% If the server receives no requests within that time, handle_info/2 is called with the atom timeout as argument.
init([]) ->
    State = [],
    Return = {ok, State, ?SLEEP_TIME},
    io:format("init: ~p~n", [State]),
    Return.

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    io:format("handle_call: ~p~n", [Return]),
    Return.

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(timeout, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.