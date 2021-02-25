%% 
%% There are four kinds of errors:
%% - compile time errors (often syntactic mistakes)
%% - logical errors
%% - run time errors
%% - generated errors
%% 
%% 
%% There are three kinds of exceptions in Erlang: errors, throws, exits
%%
%% erlang:error/1
%% erlang:exit/1
%% throw/1
%% 
%% Is 'simply' an error or a condition worthy of killing the current process? erlang:error/1 returns a stack trace and exit/1 doesn't
%% A throw is a class of exceptions used for cases that the programmer can be expected to handle.


erlang:error(badarith).
erlang:error(custom_error).
throw(permission_denied).

% TypeOfError ( error, exit, throw )

% The Expression in between try and of is said to be protected. This means that any kind of exception happening within that call will be caught.

try Expression of
  SuccessfulPattern1 [Guards] -> Expression1;
  SuccessfulPattern2 [Guards] -> Expression2
catch
  TypeOfError:ExceptionPattern1 -> Expression3;
  TypeOfError:ExceptionPattern2 -> Expression4
after % this always gets executed (no return value out of the after construct / you want to make sure a file you were reading gets closed whether exceptions are raised or not)
  Expr3
end.



%$ catch basically captures all types of exceptions.

catch throw(whoa).
% whoa

catch exit(die).
% {'EXIT',die}

catch exit(1/0).
catch doesnt:exist(a,4).
