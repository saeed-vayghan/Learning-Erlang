-module(gen_funny).

-callback foo(integer()) -> string().
-callback bar() -> ok.

-optional_callbacks([bar/0]).