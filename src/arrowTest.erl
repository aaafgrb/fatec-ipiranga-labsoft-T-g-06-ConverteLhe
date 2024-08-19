-module(arrowTest).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").
-import(arrow, [parseComposition/1, resolveComposition/1]).

% for manual testing -----------------------------------------
start() -> ok.

% run tests with {this module}:test() ------------------------------------------

parseComposition_typeConversion_test() -> 
  C = parseComposition("sabc/i123/f3.2/$concat/:123/x12"), 
  ?assertEqual([{string, "abc"}, {integer, 123}, {float, 3.2}, {function, "concat"}, {store, [1,2,3]}, {variable, [1,2]}], C).

parseComposition_forwardSlashEscape_test() -> 
  C = parseComposition("s\\/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç"), 
  ?assertEqual([{string, "/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç"}], C).

parseComposition_typeConvertError1_test() -> ?assertThrow({exception_convert_integer, "abc"}, parseComposition("iabc")).
parseComposition_typeConvertError2_test() -> ?assertThrow({exception_convert_float, "abc"}, parseComposition("fabc")).
parseComposition_typeConvertError3_test() -> ?assertThrow({exception_convert_float, "3"}, parseComposition("f3")).
parseComposition_typeConvertError4_test() -> ?assertThrow({exception_convert_store, "a123"}, parseComposition(":a123")).
parseComposition_typeConvertError5_test() -> ?assertThrow({exception_convert_variable, "a123"}, parseComposition("xa123")).

parseComposition_prefixError_test() -> ?assertThrow({exception_unsupported_prefix, "a"}, parseComposition("a123")).


resolveComposition_resolve_test() -> 
  C = resolveComposition(parseComposition("i1/f2.4/$sum")), 
  ?assertEqual({[1], [3.4]}, C(1)).

resolveComposition_functionNotFound_test() -> 
  C = resolveComposition(parseComposition("i1/f2.4/$hnuiads")), 
  ?assertThrow({exception_unsupported_function, "hnuiads"}, C(1)).

resolveComposition_notEnoughParameters_test() -> 
  C = resolveComposition(parseComposition("f2.4/$sum")), 
  ?assertThrow({exception_not_enough_parameters, "sum"}, C(1)).