-module(gen_funny).

-callback foo(integer()) -> atom().
-callback bar() -> ok.

-optional_callbacks([bar/0]).