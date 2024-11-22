-module(arrow_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-import(arrow, [parse_composition/1, resolve_composition/1]).

parseComposition_typeConversion_test() -> 
    C = parse_composition("sabc/i123/f3.2/$concat/#3/@123/x12/<a/>555"), 
    ?assertEqual([{string, <<"abc">>}, {integer, 123}, {float, 3.2}, {function, "concat"}, {inf_param, 3},
        {fin_param, [1,2,3]}, {variable, 12}, {comp_start, "a"}, {comp_end, "555"}], C).

parseComposition_forwardSlashEscape_test() -> 
    ?assertThrow({exception_convert_string, "/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç"}, parse_composition("s\\/!@#$%^&*()-+=,.<>[]{}'\"\\あ 間ç")).

parseComposition_typeConvertError1_test() -> ?assertThrow({exception_convert_integer, "abc"}, parse_composition("iabc")).
parseComposition_typeConvertError2_test() -> ?assertThrow({exception_convert_float, "abc"}, parse_composition("fabc")).
parseComposition_typeConvertError3_test() -> ?assertThrow({exception_convert_float, "3"}, parse_composition("f3")).
parseComposition_typeConvertError4_test() -> ?assertThrow({exception_convert_integer, "a123"}, parse_composition("xa123")).
parseComposition_typeConvertError5_test() -> ?assertThrow({exception_convert_integer, "a"}, parse_composition("#a")).
parseComposition_typeConvertError6_test() -> ?assertThrow({exception_convert_integer, "ad"}, parse_composition("@ad")).

parseComposition_prefixError_test() -> ?assertThrow({exception_unsupported_prefix, "a"}, parse_composition("a123")).

resolveComposition_invalidTreePos_test() -> 
    C = resolve_composition(parse_composition("i1/x12/$sum/#2")), 
    ?assertThrow({exception_inexistent_variable_position, 12}, C([1000])).

resolveComposition_badArg_test() -> 
    C = resolve_composition(parse_composition("i1/s3/$sum/#2")), 
    ?assertThrow({exception_bad_argument, [<<"3">>, 1]}, C([1000])).


resolveComposition_resolve_test() -> 
    C = resolve_composition(parse_composition("i1/f2.4/$sum/#2")), 
    ?assertEqual(3.4, C([1])).

resolveComposition_functionNotFound_test() -> 
    C = resolve_composition(parse_composition("i1/f2.4/$hnuiads/#2")), 
    ?assertThrow({exception_unsupported_function, "hnuiads"}, C([1])).

resolveComposition_notEnoughParameters_test() -> 
    C = resolve_composition(parse_composition("f2.4/$sum/#2")), 
    ?assertThrow({exception_not_enough_values_on_stack}, C([1])).

resolveComposition_curryFunc1_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$arr_list/#3/x1/$sum/$foldl/#3")),
    ?assertEqual(1090, F([1000])).

resolveComposition_curryFunc2_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$arr_list/#3/x1/$sum/#1/$map/#2")),
    ?assertEqual([1040, 1030, 1020], F([1000])).

resolveComposition_curryFunc3_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$arr_list/#3/x1/$sum/$foldl/#1/#1/#1")),
    ?assertEqual(1090, F([1000])).

resolveComposition_curryFunc4_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$arr_list/#3/x1/$sum/$foldl/#2/#1")),
    ?assertEqual(1090, F([1000])).

resolveComposition_curryFunc5_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i20/i30/i40/$arr_list/#3/x1/x1/$sum/#1/$foldl/#3")),
    ?assertThrow({exception_too_many_arguments, [40]}, F([1000])).


resolveComposition_innerComposition1_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("x1/i5/</i3/x1/$sum/#2/>/#1/$divide/@21")),
    ?assertEqual(125.0, F([1000])).

resolveComposition_innerComposition2_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i5/</x1/i2/</x2/x1/$divide/@12/>/#2/>/#1")),
    ?assertEqual(0.4, F([1000])).

resolveComposition_innerComposition3_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i5/</x1/i2/</x2/x1/$divide/@12/#2/>/#1")),
    ?assertThrow({exception_non_closing_composition}, F([1000])).

resolveComposition_innerComposition4_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i5/</>/#1")),
    ?assertThrow({exception_empty_composition}, F([1000])).


resolveComposition_parameter_test() -> 
    F = arrow:resolve_composition(arrow:parse_composition("i1/i2/i3/i4/i5/i6/i7/i8/i9/i10/i11/</x1/x2/x3/x4/x5/x6/x7/x8/x9/x10/x11/$arr_list/#11/>/#11/")),
    ?assertEqual([1,2,3,4,5,6,7,8,9,10,11], F([1000])).


-endif.