-record(either, {rl, value}). 

%
% A_{name}  -> curried function
% AT_{name} -> uncurried function
%

% util --------------------------------------------------------------

curryTuple(Func) -> fun(F) -> fun(S) -> Func({F, S}) end end.

% arrow composition -------------------------------------------------
-define(C_FIRST,  fun(Func, {F, S}) -> {Func(F), S} end).
-define(C_SECOND, fun(Func, {F, S}) -> {F, Func(S)} end).
-define(C_APPLY,  fun(Func, V) -> Func(V) end).


% arrows ------------------------------------------------------------

  % number

-define(AT_SUM,            fun({F, S})      -> F + S end).
-define(A_SUM,             curryTuple(?AT_SUM)).

-define(AT_MULTIPLY,       fun({F, S})      -> F * S end).
-define(A_MULTIPLY,        curryTuple(?AT_MULTIPLY)).

-define(AT_DIVIDE,         fun({F, S})      -> F / S end).
-define(A_DIVIDE,          curryTuple(?AT_DIVIDE)).

-define(AT_DIVIDE_BY,      fun({F, S})      -> S / F end).
-define(A_DIVIDE_BY,       curryTuple(?AT_DIVIDE_BY)).

-define(AT_POWER,          fun({F, S})      -> math:pow(F, S) end).
-define(A_POWER,           curryTuple(?AT_POWER)).

-define(AT_POWER_BY,       fun({F, S})      -> math:pow(S, F) end).
-define(A_POWER_BY,        curryTuple(?AT_POWER_BY)).

-define(A_SQRT,            fun(V)           -> math:sqrt(V) end).

  % string 

-define(AT_CONCAT_BEFORE,  fun({F, S})      -> F ++ S end).
-define(A_CONCAT_BEFORE,   curryTuple(?AT_CONCAT_BEFORE)).

-define(AT_CONCAT_AFTER,   fun({F, S})      -> S ++ F end).
-define(A_CONCAT_AFTER,    curryTuple(?AT_CONCAT_AFTER)).

-define(AT_REGEX_CAPTURE,  fun({Exp, Str}) -> 
    case re:run(Str, Exp) of
      {_, Capture} -> #either{rl = true,  value = Capture};
      nomatch      -> #either{rl = false, value = "no match"}
    end
  end).
-define(A_REGEX_CAPTURE,   curryTuple(?AT_REGEX_CAPTURE)).

-define(AT_REGEX_CAPTURED, fun({Exp, Str}) -> 
    case ?AT_REGEX_CAPTURE({Exp, Str}) of
      #either{rl = true, value = [{Cap, Len}]} -> #either{rl =  true, value = string:substr(Str, Cap + 1, Len)};
      #either{rl = _, value = _}               -> #either{rl = false,  value = "no match"}
    end
  end).
-define(A_REGEX_CAPTURED,  curryTuple(?AT_REGEX_CAPTURED)).

-define(AT_REGEX_MATCH,    fun({Exp, Str}) -> 
    case re:run(Str, Exp) of
      {Match, _} -> #either{rl = true,  value = Match};
      nomatch    -> #either{rl = false, value = "no match"}
    end
  end).
-define(A_REGEX_MATCH,     curryTuple(?AT_REGEX_MATCH)).

  % tuple

-define(A_TO_TUPLE,     fun(V) -> {V, V} end).
-define(A_INVERT_TUPLE, fun({F, S}) -> {S, F} end).


% main -----------------------------------------------------------

-define(ARROW_LIST,
[ {"sum",           ?A_SUM}
, {"multiply",      ?A_MULTIPLY}
, {"divide",        ?A_DIVIDE}
, {"divideBy",      ?A_DIVIDE_BY}
, {"power",         ?A_POWER}
, {"sqrt",          ?A_SQRT}
, {"powerBy",       ?A_POWER_BY}
, {"concatBefore",  ?A_CONCAT_BEFORE}
, {"concatAfter",   ?A_CONCAT_AFTER}
, {"regexCapture",  ?A_REGEX_CAPTURE}
, {"regexCaptured", ?A_REGEX_CAPTURED}
, {"regexMatch",    ?A_REGEX_MATCH}
, {"toTuple",       ?A_TO_TUPLE}
, {"invertTuple",   ?A_INVERT_TUPLE}
]).

