-module(typesTest).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

% for manual testing -----------------------------------------
start() -> ?STR_TO_INTEGER("a").

% run tests with {this module}:test() ------------------------------------------

strToInteger1_test() -> ?assertEqual(0, ?STR_TO_INTEGER("0")).
strToInteger2_test() -> ?assertEqual(-0, ?STR_TO_INTEGER("-0")).
strToInteger3_test() -> ?assertEqual(-1, ?STR_TO_INTEGER("-1")).
strToInteger4_test() -> ?assertEqual(576460752303423488576460752303423488, ?STR_TO_INTEGER("576460752303423488576460752303423488")).
strToInteger5_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, ?STR_TO_INTEGER("2.99999999999")).
strToInteger6_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, ?STR_TO_INTEGER("3aaasdasd")).
strToInteger7_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, ?STR_TO_INTEGER("aaasdasd")).

strToFloat1_test() -> ?assertEqual(0.0, ?STR_TO_FLOAT("0.0")).
strToFloat2_test() -> ?assertEqual(-0.0, ?STR_TO_FLOAT("-0.0")).
strToFloat3_test() -> ?assertEqual(-1.0, ?STR_TO_FLOAT("-1.0")).
strToFloat4_test() -> ?assertEqual(57646075230342348857646.0752303423488, ?STR_TO_FLOAT("57646075230342348857646.0752303423488")).
strToFloat5_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, ?STR_TO_FLOAT("2.4asdafas")).
strToFloat6_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, ?STR_TO_FLOAT("asdafas")).

identity1_test() -> ?assertEqual("0.0", ?IDENTITY("0.0")).
identity2_test() -> ?assertEqual(0, ?IDENTITY(0)).
identity3_test() -> ?assertEqual(asd, ?IDENTITY(asd)).