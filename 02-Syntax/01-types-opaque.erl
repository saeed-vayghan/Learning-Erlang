%% module1.erl
-type my_tup1() :: {any(), any()}.
-opaque my_tup2() :: {any(), any()}.

-export_type([my_tup1/0, my_tup2/0]).



%% module2.erl
-spec foo1(module1:my_tup1()) -> ok.
foo1({_, _}) -> ok. %% fine

-spec foo2(module1:my_tup2()) -> ok.
foo2({_, _}) -> ok. 
%% Dialyzer warning, because you are looking at the internal structure of a my_tup2() term.
%% If you defined the same function in the module module1, it wouldn't give a warning.

foo2(_) -> ok. %% no warning again.


%% there is no meaning in declaring a type as opaque if you don't export it.