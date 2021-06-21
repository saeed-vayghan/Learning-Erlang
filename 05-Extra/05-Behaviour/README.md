#### Behaviour is basically a way for a module to specify functions it expects another module to have.
#### Behaviour is a way of formatting common patterns in process-oriented programming.
#### `gen_server` OTP behaviour takse server-client pattern and devides it into two halves, `generic` part and `app-specific` implementation part.


* The behaviour is the contract sealing the deal between the well-behaved generic part of the code and the specific, error-prone part of the code (yours).
* You can define your own behaviour by adding `-callback` directives in your module.

<hr>

##### If you want to use this behaviour, you need to have a `foo function` that takes an integer and returns an atom.
```
% filename: my_behaviour.erl

-module(my_behaviour).

-callback foo(integer()) -> atom().
-callback bar() -> ok.

-optional_callbacks([bar/0]).
```

#### If you use this behaviour in another module, the `compiler` will warn if it does not `export` `foo/1`, and `Dialyzer` will warn if the types are not correct. With this module:

```
% filename: bar.erl

-module(bar).
-behaviour(my_behaviour).
-export([foo/1]).

foo([]) ->
  some_atom.


%% How to test it:
> dialyzer --src bar.erl my_behaviour.erl
```

<hr>

1. More: https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
1. More: http://20bits.com/article/erlang-a-generic-server-tutorial