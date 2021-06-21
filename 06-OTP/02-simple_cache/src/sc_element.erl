-module(sc_element).

-behaviour(gen_server).

%% API
-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% Constants
-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).


%%%===================================================================
%%% Public API
%%%===================================================================

% The first thing that happens when your sc_element process starts is that the gen_server callback function init/1 is called to initialize the process.
start_link(Value, LeaseTime) ->
  gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
  % starting a child process should be done by asking the supervisor
  sc_sup:start_child(Value, LeaseTime).

create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
  gen_server:cast(Pid, delete).


%%%===================================================================
%%% Helpers
%%%===================================================================

time_left(_StartTime, infinity) ->
  infinity;
time_left(StartTime, LeaseTime) ->
  Now         = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 -> 0;
    Time                -> Time * 1000
  end.


%%%===================================================================
%%% gen_server callbacks (Private API)
%%%===================================================================

init([Value, LeaseTime]) ->
  Now       = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  { ok
  , #state{value = Value, lease_time = LeaseTime, start_time = StartTime}
  , time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From,  State) ->
  #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
  {stop, normal, State}.

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  sc_store:delete(self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
