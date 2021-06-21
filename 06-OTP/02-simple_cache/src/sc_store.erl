-module(sc_store).

%% API
-export([init/0, insert/2, delete/1, lookup/1]).

%% Constants
-define(TABLE_ID, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] -> {ok, Pid};
    []           -> {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).
