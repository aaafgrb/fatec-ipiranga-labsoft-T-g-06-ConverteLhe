-module(arrow_test).
-ifdef(TEST).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").
-import(arrow, [parse_composition/1, resolve_composition/1]).

% for manual testing -----------------------------------------
start() -> ok.

% run tests with {this module}:test() ------------------------------------------

parseComposition_typeConversion_test() -> 
    C = parse_composition("sabc/i123/f3.2/$concat/#3/:123/x12"), 
    ?assertEqual([{string, "abc"}, {integer, 123}, {float, 3.2}, {function, "concat"}, {funcApply, 3}, {store, [1,2,3]}, {variable, [1,2]}], C).

parseComposition_forwardSlashEscape_test() -> 
    C = parse_composition("s\\/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç"), 
    ?assertEqual([{string, "/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç"}], C).

parseComposition_typeConvertError1_test() -> ?assertThrow({exception_convert_integer, "abc"}, parse_composition("iabc")).
parseComposition_typeConvertError2_test() -> ?assertThrow({exception_convert_float, "abc"}, parse_composition("fabc")).
parseComposition_typeConvertError3_test() -> ?assertThrow({exception_convert_float, "3"}, parse_composition("f3")).
parseComposition_typeConvertError4_test() -> ?assertThrow({exception_convert_store, "a123"}, parse_composition(":a123")).
parseComposition_typeConvertError5_test() -> ?assertThrow({exception_convert_store, "-1"}, parse_composition(":-1")).
parseComposition_typeConvertError6_test() -> ?assertThrow({exception_convert_variable, "a123"}, parse_composition("xa123")).
parseComposition_typeConvertError7_test() -> ?assertThrow({exception_convert_variable, "-11"}, parse_composition("x-11")).
parseComposition_typeConvertError8_test() -> ?assertThrow({exception_convert_integer, "a"}, parse_composition("#a")).

parseComposition_prefixError_test() -> ?assertThrow({exception_unsupported_prefix, "a"}, parse_composition("a123")).

resolveComposition_invalidTreePos1_test() -> 
    C = resolve_composition(parse_composition("i1/x12/$sum/#2")), 
    ?assertThrow({exception_inexistent_variable_position, [1, 2]}, C(1000)).

resolveComposition_invalidTreePos2_test() -> 
    C = resolve_composition(parse_composition("i1/:12")), 
    ?assertThrow({exception_inexistent_variable_position, [1,2]}, C(1000)).

resolveComposition_badArg_test() -> 
    C = resolve_composition(parse_composition("i1/s3/$sum/#2")), 
    ?assertThrow({exception_bad_argument, ["3", 1]}, C(1000)).


resolveComposition_resolve_test() -> 
    C = resolve_composition(parse_composition("i1/f2.4/$sum/#2")), 
    ?assertEqual({[1], [3.4]}, C(1)).

resolveComposition_functionNotFound_test() -> 
    C = resolve_composition(parse_composition("i1/f2.4/$hnuiads/#2")), 
    ?assertThrow({exception_unsupported_function, "hnuiads"}, C(1)).

resolveComposition_notEnoughParameters_test() -> 
    C = resolve_composition(parse_composition("f2.4/$sum/#2")), 
    ?assertThrow({exception_not_enough_values_on_stack, [2.4]}, C(1)).

resolveComposition_curryFunc1_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$list/#3/x1/$sum/$foldl/#3")),
    ?assertEqual({[1000], [1090]}, F(1000)).

resolveComposition_curryFunc2_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$list/#3/x1/$sum/#1/$map/#2")),
    ?assertEqual({[1000], [[1040, 1030, 1020]]}, F(1000)).

resolveComposition_curryFunc3_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$list/#3/x1/$sum/$foldl/#1/#1/#1")),
    ?assertEqual({[1000], [1090]}, F(1000)).

resolveComposition_curryFunc4_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$list/#3/x1/$sum/$foldl/#2/#1")),
    ?assertEqual({[1000], [1090]}, F(1000)).

resolveComposition_curryFunc5_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$list/#3/x1/x1/$sum/#1/$foldl/#3")),
    ?assertThrow({exception_too_many_arguments, [40]}, F(1000)).

-endif.