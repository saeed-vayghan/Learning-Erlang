%%%% This source code create a simple key/value store service based on map Erlang datastructure. %%%%

-module(gen_server_4).
-behaviour(gen_server).

%% API
-export([ start_link/0
        , get/1
        , get/2
        , state/0
        , delete/1
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



%%%============================================================================
%%% Public API
%%%
%%% Key/Value database is a simple store, value indexed by an unique key. 
%%% This implementation is based on map, this datastructure is like hash table
%%%
%%% put/2
%%% Put a value indexed by a key.
%%% We assume the link is stable and the data will be written.
%%% So we use an asynchronous call with gen_server:cast/2.
%%%============================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put(Key, Value) ->
  gen_server:cast(?MODULE, {put, {Key, Value}}).

%% get/1
%% Takes one argument, a key and will a return the value indexed by this same key.
%% We use a synchronous call with gen_server:call/2.

get(Key) ->
  gen_server:call(?MODULE, {get, Key}).


%% delete/1 
%% Like `put/1`, we assume the data will be removed. So, we use an asynchronous call with gen_server:cast/2.

delete(Key) ->
  gen_server:cast(?MODULE, {delete, Key}).


%% state/0 
%% This function will return the current state (here the map who contain all indexed values)
%% We need a synchronous call.

state() ->
  gen_server:call(?MODULE, {get_state}).


%% stop/0
%% This function stop gen_server_4 server process.

stop() ->
  gen_server:stop(?MODULE).


%%%===================================================================
%%% gen_server callbacks (Private API)
%%%===================================================================

%% init/1
%% Here init/1 will initialize state with simple empty map datastructure.

init([]) ->
  {ok, #{}}.


%% handle_call/3
%% Now, we need to define our handle. In a gen_server_4 server we need to get our 
%% value from a key, this feature need to be synchronous, so, using handle_call seems a good choice:

handle_call({get, Key}, From, State) ->
  Response = maps:get(Key, State, undefined),
  {reply, Response, State};
handle_call({get_state}, From, State) ->
  Response = {current_state, State},
  {reply, Response, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.


%% handle_cast/2
%% put/2 will execute this function.
%% delete/1 will execute this function.

handle_cast({put, {Key, Value}}, State) ->
  NewState = maps:put(Key, Value, State),
  {noreply, NewState};
handle_cast({delete, Key}, State) ->
  NewState = maps:remove(Key, State),
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.



%% We don't need other features, other handlers do nothing.
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




% c(gen_server_4).
% gen_server_4:start_link().

% get current store. will return: #{}
% gen_server_4:state().

% put some data
% gen_server_4:put(1, one).
% gen_server_4:put(hello, bonjour).
% gen_server_4:put(list, []).

% get current store. will return: #{1 => one,hello => bonjour,list => []}
% gen_server_4:state().

% delete a value
% gen_server_4:delete(1).
% gen_server_4:state(). Will return: #{1 => one,hello => bonjour,list => []}

% stopping gen_server_4 server
% gen_server_4:stop().