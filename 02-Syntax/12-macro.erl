% https://erlang.org/doc/reference_manual/macros.html

% Predefined Macros
% The following macros are predefined:

% ?MODULE
%   % The name of the current module.

% ?MODULE_STRING.
%   % The name of the current module, as a string.

% ?FILE.
%   % The file name of the current module.

% ?LINE.
%   % The current line number.

% ?MACHINE.
%   % The machine name, 'BEAM'.

% ?FUNCTION_NAME
  % The name of the current function.

% ?FUNCTION_ARITY
  % The arity (number of arguments) for the current function.

% ?OTP_RELEASE
  % The OTP release that the currently executing ERTS application is part of, as an integer. 



%%%%%%%%%%%%%%%%%%%% Flow Control in Macros %%%%%%%%%%%%%%%%%%%%

-undef(Macro).
  % Causes the macro to behave as if it had never been defined.

-ifdef(Macro).
  % Evaluate the following lines only if Macro is defined.

-ifndef(Macro).
  % Evaluate the following lines only if Macro is not defined.

-else.
  % Only allowed after an ifdef or ifndef directive. If that condition is false, the lines following else are evaluated instead.

-endif.
  % Specifies the end of an ifdef, an ifndef directive, or the end of an if or elif directive.

-if(Condition).
  % Evaluates the following lines only if Condition evaluates to true.

-elif(Condition).
  % Only allowed after an if or another elif directive. If the preceding if or elif directives do not evaluate to true, and the Condition evaluates to true, the lines following the elif are evaluated instead.

-ifdef(TEST).
-export([my_testinh_function/arity]).
-endif.





%%%%%%%%%%%%%%%%%%%% Stringifying Macro Arguments %%%%%%%%%%%%%%%%%%%%

% The construction ??Arg, where Arg is a macro argument, is expanded to a string containing the tokens of the argument.

-define(TESTCALL(Call), io:format("Call ~s: ~w~n", [??Call, Call])).
?TESTCALL(myfunction(1, 2)),
?TESTCALL(you:function(2, 1)).

% results in -->

io:format("Call ~s: ~w~n",["myfunction ( 1 , 2 )", myfunction(1, 2)]),
io:format("Call ~s: ~w~n",["you : function ( 2 , 1 )",y ou:function(2, 1)]).


%%%%%%%%%%%%%%%%%%%% its traditional to use all uppercase for constant macros and all lowercase for other macros. %%%%%%%%%%%%%%%%%%%%

-define(PI, 3.14).
-define(pair(x,y), {x, y})

-define(foo, true).
-undef(foo).
-define(foo, false).


-ifdef(DEBUG) %ifndef
-define(show(X), if:format("X = ~p", [X])).
-else.
-define(show(X), ok).
-endif.


curren_pos() -> [{module, ?MODULE}, {file, ?FILE}, {line, ?LINE}]
-define(TIMEOUT, 200).
-define(MACRO1(X, Y), {a, X, b, Y}).

bar(X) ->
  ?MACRO1(a, b),
  ?MACRO1(X, 123)
