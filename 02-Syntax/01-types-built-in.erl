
% is_atom/1           is_binary/1        
% is_bitstring/1      is_boolean/1        is_builtin/3       
% is_float/1          is_function/1       is_function/2      
% is_integer/1        is_list/1           is_number/1        
% is_pid/1            is_port/1           is_record/2        
% is_record/3         is_reference/1      is_tuple/1   


% conversions

% atom_to_binary/2, atom_to_list/1, binary_to_atom/2,
% binary_to_existing_atom/2, binary_to_list/1,
% bitstring_to_list/1, binary_to_term/1, float_to_list/1, fun_to_list/1,
% integer_to_list/1, integer_to_list/2,
% iolist_to_binary/1, iolist_to_atom/1,
% list_to_atom/1, list_to_binary/1, list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1, list_to_integer/2,
% list_to_pid/1, list_to_tuple/1,
% pid_to_list/1, port_to_list/1, ref_to_list/1,
% term_to_binary/1, term_to_binary/2,
% tuple_to_list/1.


% built-in types

% any()         Any Erlang term at all. ( === term(), _ )
% byte()	    Defined as 0..255, its any valid byte in existence
% reference()   Unique values returned by make_ref() or erlang:monitor/2.
% boolean() string() iolist() module()
% timeout() === non_neg_integer() | 'infinity'
% no_return(), none()	no term or type is valid, if any functions return value type is none(), it means the function should crash.
% atom() integer() non_neg_integer() pos_integer() neg_integer() float() tuple() {Type1, Type2, ..., TypeN}
% number() === integer() | float()


% []                An empty list.
% [Type()] 	        A list containing a given type
% [Type(), ...]	    This special case of [Type()] mentions that the list can not be empty.
% [integer()] list() === [any()]
% The improper list [1, 2 | a] could be typed as improper_list(integer(), atom())
% maybe_improper_list() === maybe_improper_list(any(), any()) 

% fun() 
% fun((...) -> Type)	An anonymous function of any arity that returns a given type. A given function that returns lists could be noted as fun((...) -> list()).
% fun(() -> Type)	    An anonymous function with no arguments, returning a term of a given type.
% fun((Type1, Type2, ..., TypeN) -> Type)	An anonymous function taking a given number of arguments of a known type. An example could be a function that handles an integer and a floating point value, which could be declared as fun((integer(), float()) -> any()).
