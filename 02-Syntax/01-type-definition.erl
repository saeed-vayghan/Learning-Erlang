-module(types).



% type definitation
% -type TypeName() :: TypeDefinition.
% -opaque my_opaq_type() :: TypeDefinition.


-type mydictionary(Key, Val) :: [{Key, Val}].


-type tree() :: {'node', Left::tree(), Right::tree(), Key::any(), Value::any()}.

-record(user, { name = "" :: string()
              , notes :: tree()
              , age :: non_neg_integer()
              , friends=[] :: [#user{}]
              , bio :: string() | binary()}).



-record(userx, {name = "" :: string()
               , notes :: tree()
               , age :: non_neg_integer()
               , friends=[] :: [userx()]
               , bio :: string() | binary()}).

-type userx() :: #userx{}.



-type suit() :: spades | clubs | hearts | diamonds.
-type value() :: 1..10 | j | q | k.
-type card() :: {suit(), value()}.



-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'mul',expr(),expr()}.



% Exporting Types
-export_type([TypeName/Arity]).
