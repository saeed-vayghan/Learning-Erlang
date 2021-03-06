#### It's a bad idea to program defensively in Erlang. Why?

The point of the recommendation is that allowing an Erlang process to exit when things go wrong inside the Erlang program is a good approach.
For example, when parsing an integer it makes perfect sense to just write:
If L is not an integer, the process will exit and a supervisor somewhere will restart that part of the system.

```
> I = list_to_integer(L)
```

If a more descriptive diagnostic is required, use a manual exit: 

```
uppercase_ascii(C) when C >= $a, C =< $z ->
  C - ($a - $A);
uppercase_ascii(X) ->
  exit({"uppercase_ascii given non-lowercase argument", X}).
```

#### This separation of error detection and error handling is a key part of Erlang.
It reduces complexity in fault-tolerant systems by keeping the normal and error-handling code separate.
As for most advice, there are exceptions to the recommendation: One example is the case where input is coming from an untrusted interface => e.g. a user or an external program. 