-module(main_model_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

main_list_test() -> ?assertEqual({true, [<<"4">>, <<"5">>, <<"6">>]}, 
    main_model:main([["1"], ["2"], ["3"]], "x1/$toInt/#1/i3/$sum/#2")).

main_empty_test() -> ?assertEqual({true, []}, 
    main_model:main([], "x1/$toInt/#1/i3/$sum/#2")).

main_exception1_test() -> 
    ?assertEqual({false, <<"unsupported prefix: w">>}, 
        main_model:main([["1"], ["2"], ["3"]], "w1/$toInt/#1/i3/$sum/#2")).

main_exception2_test() -> 
    ?assertEqual({false, <<"unsupported function: w">>}, 
        main_model:main([["1"], ["2"], ["3"]], "x1/$toInt/#1/i3/$w/#2")).

main_exception3_test() -> 
    ?assertEqual({false, <<"failed convertion to float: 3">>}, 
        main_model:main([["1.0"], ["2.1"], ["3"]], "x1/$toFloat/#1/i3/$sum/#2")).

main_exception4_test() -> 
    ?assertEqual({false, <<"failed convertion to integer: 1.0">>}, 
        main_model:main([["1.0"], ["2.1"], ["3"]], "x1/$toInt/#1/i3/$sum/#2")).

main_exception5_test() -> 
    ?assertEqual({false, <<"failed convertion to integer: h">>}, 
        main_model:main([["1"], ["2"], ["3"]], "xh/$toInt/#1/i3/$sum/#2")).

main_exception6_test() -> 
    ?assertEqual({false, <<"inexistent variable position: 2">>}, 
        main_model:main([["1"], ["2"], ["3"]], "x2/$toInt/#1/i3/$sum/#2")).

main_exception7_test() -> 
    ?assertEqual({false, <<"failed function application to arguments: [\"s\",1]">>}, 
        main_model:main([["1"], ["2"], ["3"]], "x1/$toInt/#1/ss/$sum/#2")).

main_exception8_test() -> 
    ?assertEqual({false, <<"not enough values on the stack">>}, 
        main_model:main([["1"], ["2"], ["3"]], "x1/$toInt/#1/$sum/#2")).

main_exception9_test() -> 
    ?assertEqual({false, <<"too many arugments applied to function: [1]">>}, 
        main_model:main([["1"], ["2"], ["3"]], "x1/$toInt/#1/i3/i4/$sum/#3")).

main_exception10_test() -> 
    ?assertEqual({false, <<"non closing composition">>}, 
        main_model:main([["1"], ["2"], ["3"]], "</x1/$toInt/#1/i3/i4/$sum/#3")).

main_exception11_test() -> 
    ?assertEqual({false, <<"empty composition">>}, 
        main_model:main([["1"], ["2"], ["3"]], "</>/x1/$toInt/#1/i3/i4/$sum/#3")).


-endif.