% This source code create a simple key/value store service based on map Erlang datastructure. Firstly, we need to define all information concerning our gen_server:



-module(cache).
-behaviour(gen_server).

% our API
-export([start_link/0]).
-export([get/1, put/2, state/0, delete/1, stop/0]).

% our handlers
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Defining our function to start `cache` process:

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%
% API

% Key/Value database is a simple store, value indexed by an unique key. 
% This implementation is based on map, this datastructure is like hash 
%  in Perl or dictionaries in Python. 

% put/2
% put a value indexed by a key. We assume the link is stable 
% and the data will be written, so, we use an asynchronous call with 
% gen_server:cast/2.

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, {Key, Value}}).

% get/1
% take one argument, a key and will a return the value indexed 
% by this same key. We use a synchronous call with gen_server:call/2.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

% delete/1 
% like `put/1`, we assume the data will be removed. So, we use an 
% asynchronous call with gen_server:cast/2.

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

% state/0 
% This function will return the current state (here the map who contain all 
% indexed values), we need a synchronous call.

state() ->
    gen_server:call(?MODULE, {get_state}).

% stop/0
% This function stop cache server process.

stop() ->
    gen_server:stop(?MODULE).

%%%%%%%%%%%%%%%
% Handlers

% init/1
% Here init/1 will initialize state with simple empty map datastructure.

init([]) ->
    {ok, #{}}.

% handle_call/3
% Now, we need to define our handle. In a cache server we need to get our 
% value from a key, this feature need to be synchronous, so, using 
% handle_call seems a good choice:

handle_call({get, Key}, From, State) ->
    Response = maps:get(Key, State, undefined),
    {reply, Response, State};

% We need to check our current state, like get_fea

handle_call({get_state}, From, State) ->
    Response = {current_state, State},
    {reply, Response, State};

% All other messages will be dropped here.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% handle_cast/2
% put/2 will execute this function.

handle_cast({put, {Key, Value}}, State) ->
    NewState = maps:put(Key, Value, State),
    {noreply, NewState};

% delete/1 will execute this function.

handle_cast({delete, Key}, State) ->
    NewState = maps:remove(Key, State),
    {noreply, NewState};

% All other messages are dropped here.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%
% other handlers

% We don't need other features, other handlers do nothing.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






% % compile cache
% c(cache).

% % starting cache server
% cache:start_link().

% % get current store
% % will return:
% %   #{}
% cache:state().

% % put some data
% cache:put(1, one).
% cache:put(hello, bonjour).
% cache:put(list, []).

% % get current store
% % will return:
% %   #{1 => one,hello => bonjour,list => []}
% cache:state().

% % delete a value
% cache:delete(1).
% cache:state().
% %   #{1 => one,hello => bonjour,list => []}

% % stopping cache server
% cache:stop().