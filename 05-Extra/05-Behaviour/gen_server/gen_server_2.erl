%%%% Here is an example of a service that greets people by the given name, and keeps track of how many users it encountered. See usage below. %%%%
%%%% Greets people and counts number of times it did so. %%%%

-module(gen_server_3).
-behaviour(gen_server).

%% API
-export([ start_link/0
        , greet/1
        , get_count/0
        , stop/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% Constants
-define(SERVER, ?MODULE).

-record(state, {count :: integer()}).



%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

greet(Name) ->
  gen_server:cast(?MODULE, {greet, Name}).

get_count() ->
  gen_server:call(?MODULE, {get_count}).

stop() ->
  gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks (Private API)
%%%===================================================================

init({}) ->
  {ok, #state{count = 0}}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({greet, Name}, #state{count = Count} = State) ->
  io:format("Greetings ~s!~n", [Name]),
  {noreply, State#state{count = Count + 1}};
handle_cast(Msg, State) ->
  error_logger:warning_msg("Bad message: ~p~n", [Msg]),
  {noreply, State}.

handle_call({get_count}, _From, State) ->
  {reply, {ok, State#state.count}, State};
handle_call(Request, _From, State) ->
  error_logger:warning_msg("Bad message: ~p~n", [Request]),
  {reply, {error, unknown_call}, State}.

handle_info(Info, State) ->
  error_logger:warning_msg("Bad message: ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Test
%%%===================================================================

start_test() ->
  {ok, _} = gen_server_3:start_link().


% 1> c(gen_server_3).
% {ok,gen_server_3}

% 2> gen_server_3:start_link().
% {ok,<0.62.0>}

% 3> gen_server_3:greet("Andriy").
% Greetings Andriy!
% ok

% 4> gen_server_3:greet("Mike").
% Greetings Mike!
% ok

% 5> gen_server_3:get_count().
% {ok,2}
