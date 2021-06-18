
% https://erlang.org/doc/reference_manual/typespec.html

% type definitation
% -type TypeName() :: TypeDefinition.
% -opaque my_opaq_type() :: TypeDefinition.


-type redirect_url_field()   :: atom() | binary().
-type capture_field()        :: pending | success.
-type parameters_field()     :: map().
-type mydictionary(Key, Val) :: [{Key, Val}].


-type tree() :: { 'node'
                , Left  :: tree()
                , Right :: tree()
                , Key   :: any()
                , Value :: any()}.

-record(user, { name = ""    :: string()
              , nodes        :: tree()
              , age          :: non_neg_integer()
              , friends = [] :: [#user{}]
              , bio          :: string() | binary()}).



-record(userx, { name = ""    :: string()
               , nodes        :: tree()
               , age          :: non_neg_integer()
               , friends = [] :: [userx()]
               , bio          :: string() | binary()}).

-type userx() :: #userx{}.



-type suit()  :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | j | q | k.
-type card()  :: {suit(), value()}.



-type expr() :: {'num', integer()}
             |  {'var', atom()}
             |  {'add', expr(), expr()}
             |  {'mul', expr(), expr()}.



% Exporting Types
-export_type([TypeName/Arity]).
