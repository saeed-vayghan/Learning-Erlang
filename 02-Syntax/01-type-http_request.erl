
-type method() :: get | post.
-type service() :: binary().
-type body() :: binary().
-type headers() :: [{atom(), binary() | integer()}].
-type statuscode() :: non_neg_integer().

-spec http_request(service(), method(), uri_string:uri_string(), headers(), body(), Opts) ->
  {ok, statuscode(), headers(), body()} | {error, term()} | {error, timeout} when
  Opts :: #{ timeout => non_neg_integer()}.