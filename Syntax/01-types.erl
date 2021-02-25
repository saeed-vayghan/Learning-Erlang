-module(module_name).

-export([variables/0]).


%% Number      − Integers and Floats.
%% Atom        − An atom is a literal, a constant with name.
%% Boolean     − True or False
%% Bit String  − A bit string is used to store an area of un-typed memory.
%% List        − A list is a compound data type with a variable number of elements.
%% Tuple       − A tuple is a compound data type with a fixed number of elements.
%% Map         − A map is a compound data type with a variable number of key-value associations.

%% _ (underscore) - The variable we do not care about it.

%%% There are pitfalls to using atoms for too many things: an atom is referred to in an "atom table" which consumes
%%% memory (4 bytes/atom in a 32-bit system, 8 bytes/atom in a 64-bit system).
%%% The atom table is not garbage collected, and so atoms will accumulate until the system tips over,
%%% either from memory usage or because 1048577 atoms were declared.

% Reserved atoms: after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor

%% Note: the boolean operators "and" and "or" will always evaluate arguments on both sides of the operator.
%% If you want to have the short-circuit operators (which will only evaluate the right-side argument if it needs to),
%% use "andalso" and "orelse".




variables() ->
  Num = 123,

  %% Erlang represents strings as lists of integers. ( to turn off this behaviour ==> shell:strings(false). ) 
  %% [65, 66, 67].
  %% "ABC"

  %% [0, 65, 66, 67].
  %% [0,65,66,67]


  Atom_0 = abc,
  Atom_1 = any_string,
  Atom_2 = 'Any_string',
  Atom_3 = 'Hello there!',

  Tuple_1 = {a, 12, b},
  Tuple_2 = {},
  Tuple_3 = {1, 2, a, 4, b},
  Size = tuple_size(Tuple_3),

  List_1 = [],
  List_2 = [a, b, 12],
  List_3 = [22],
  List_4 = [a, 'hello friend'],

  X = { book, preface, acknowledgments, contents, { chapters, [ {chapter, 1, 'An Erlang Tutorial'}, {chapter, 2} ] } },

  N = {12, banana}. % {12,banana}
  {A, B} = N. % {12,banana}
  A. % 12
  B. % banana
  
  {A, B} = {[1, 2, 3],  {x, y}}. % {[1, 2, 3], {x, y}}
  A. % [1,2,3]
  B. % {x,y}

  [a, X, b, Y] = [a, {hello, fred}, b, 1]. % [a,{hello,fred},b,1]
  X. % {hello,fred}
  Y. % 1

  {_, L, _} = {fred, {likes, [wine, women, song]}, {drinks, [whisky, beer]}}. % {fred,{likes,[wine,women,song]},{drinks,[whisky,beer]}}
  L. % {likes,[wine,women,song]}


  {C, [Head|Tail]} = {{222, man}, [a,b,c]}
  % C → {222, man} , Head → a , Tail → [b, c].

  {A, foo, A} = {123, foo, 123} % succeeds
  {A, foo, A} = {123, foo, bar} % failes

  [{person, Name, Age, _}|T] = [{person, fred, 22, male}, {person, susan, 19, female}, {person, rachel, 20, female}]


  %% Bit String
  Bin1 = <<10,20>>, %% a Bit String consisting of 2 bits.
  X = binary_to_list(Bin1),
  io:fwrite("~w",[X]).


  %% Map
  M1 = #{name=>john, age=>25},


  % $<Char> represents the ASCII value of the character Char
  $A, % 65

  % <Base>#<Value>
  16#ffff, % 65535

  % A = B = C = D is parsed as A = (B = (C = D))


  true and false. % false
  false or true. % true
  true xor false. % true
  not false. % true
  not (true and true). % false