-module(arrowTest).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").
-include("throws.hrl").
-import(arrow, [parseArrow/1, applyArguments/2, castArgumentType/1, parseComposition/1]).

% for manual testing -----------------------------------------
start() -> castArgumentType("ii").

% run tests with {this module}:test() ------------------------------------------

parseComposition1_test() -> T = parseComposition("|sum/i3|sum/f9.2"), ?assertEqual(13.2, T(1)).
parseComposition2_test() -> T = parseComposition("|concatAfter/i3|"), ?assertEqual("asdi3", T("asd")).

% ------------------------------------------------------------------------------


% sum
parseArrow1sum_test() -> ?assertEqual(5, parseArrow("sum/i2/i3")).
parseArrow2sum_test() -> ?assertEqual(5, parseArrow("sum/i2/i3/")).
parseArrow3sum_test() -> ?assertEqual(6.0, parseArrow("sum/f2.5/f3.5")).
parseArrow4sum_test() -> ?assertEqual(5.5, parseArrow("sum/f2.5/i3")).
parseArrow5sum_test() -> ?assertThrow(?EX_UNSUPPORTED_TYPE, parseArrow("sum/f2.5/3")).
parseArrow6sum_test() -> ?assertError(badarith, parseArrow("sum/f2.5/sasd")).
parseArrow7sum_test() -> ?assertEqual(5, parseArrow("sum/i2//i3")).

% concatAfter
parseArrow1concatAfter_test() -> ?assertEqual("i3i2", parseArrow("concatAfter/si2/si3")).
parseArrow2concatAfter_test() -> ?assertEqual("i3i/2", parseArrow("concatAfter/si\\/2/si3")).

% -----------------------------------------------------------------------------

applyArguments1_test() -> ?assertEqual(1, applyArguments(fun(F) -> F end, [1])).
applyArguments2_test() -> ?assertError({badfun, _}, applyArguments(fun(F) -> F end, [1, 1])).
applyArguments3_test() -> ?assertEqual(3, applyArguments(fun(F) -> fun(S) -> F + S end end, [1, 2])).
applyArguments4_test() -> ?assertError(badarith, applyArguments(fun(F) -> fun(S) -> F + S end end, [1, "a"])).

% -----------------------------------------------------------------------------

castArgumentTypei1_test() -> ?assertEqual(0, castArgumentType("i0")).
castArgumentTypei2_test() -> ?assertEqual(-0, castArgumentType("i-0")).
castArgumentTypei3_test() -> ?assertEqual(-10, castArgumentType("i-10")).
castArgumentTypei4_test() -> ?assertEqual(4123124512312312341231245123123123, castArgumentType("i4123124512312312341231245123123123")).
castArgumentTypei5_test() -> ?assertThrow(?EX_UNSUPPORTED_TYPE, castArgumentType("10")).
castArgumentTypei6_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, castArgumentType("ii")).
castArgumentTypei7_test() -> ?assertThrow(?EX_EMPTY_ARGUMENT, castArgumentType("")).

castArgumentTypef1_test() -> ?assertEqual(0.0, castArgumentType("f0.0")).
castArgumentTypef2_test() -> ?assertEqual(-0.0, castArgumentType("f-0.0")).
castArgumentTypef3_test() -> ?assertEqual(-10.3, castArgumentType("f-10.3")).
castArgumentTypef4_test() -> ?assertEqual(4123124512312312341231245123123123.123123123, castArgumentType("f4123124512312312341231245123123123.123123123")).
castArgumentTypef5_test() -> ?assertThrow(?EX_UNSUPPORTED_TYPE, castArgumentType("10.0")).
castArgumentTypef6_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, castArgumentType("fi")).
castArgumentTypef7_test() -> ?assertThrow(?EX_TYPE_PARSE_ERROR, castArgumentType("f0")).

castArgumentTypes1_test() -> ?assertEqual("0123", castArgumentType("s0123")).
castArgumentTypes2_test() -> ?assertEqual("isd", castArgumentType("sisd")).
castArgumentTypes3_test() -> ?assertThrow(?EX_UNSUPPORTED_TYPE, castArgumentType("10f")).
castArgumentTypes4_test() -> ?assertEqual("!@#$%^&*()_+{}[]-=|\'\"/,.<>", castArgumentType("s!@#$%^&*()_+{}[]-=|\'\"/,.<>")).

