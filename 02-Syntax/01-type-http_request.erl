
-type method()  :: get | post.
-type service() :: binary().
-type body()    :: binary().
-type headers() :: [{atom(), binary() | integer()}].
-type code()    :: non_neg_integer().

-type http_error_result() :: {error, {code(), body(), [headers()]}}
                          |  {error, term() | timeout}.

-type http_result() :: {ok, map()} | http_error_result().


-spec http_request( service()
                  , method()
                  , uri_string:uri_string()
                  , headers()
                  , body()
                  , Opts) ->
  {ok, code(), headers(), body()} | {error, term()} | {error, timeout} when
  Opts :: #{timeout => non_neg_integer()}.