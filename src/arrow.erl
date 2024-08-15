-module(arrow).
-export([parseComposition/1, parseArrow/1, applyArguments/2, castArgumentType/1]).
-include("arrow.hrl").
-include("types.hrl").

% receives a String (format: '|{functionName}/{type}{arguments}{0..n}|')
% ex |sum/i1/i3| = 4

% escape characters with \

% |func1/p1/p2|func2/p3|
% ((func1 <- p1) <- p2) -> (func2 <- p3)

% add other operators (like '|') for first and second function apply
% example if $ is first -> $$ is first first -> expects {{_, _}, _}

% mudar separadores? 
%   \ e | nao funcionam por algum motivo

% parse an arrow composition
parseComposition(Str) -> 
  %escape characters after \
  Escaped = escapeKeySymbols(Str),

  %split by firt second and general splitters
  %  put them in a tuple array

  Split = string:tokens(Str, "|"),
  fun(V) -> lists:foldl(fun(E, A) -> ?C_APPLY(E, A) end, V, [ parseArrow(X) || X <- Split]) end.

escapeKeySymbols(Str) -> string:str(Str, "2").

% parse a single arrow
parseArrow(Str) -> 
  [Head | Tail] = string:tokens(Str, "/"),

  case lists:keyfind(Head, 1, ?ARROW_LIST) of
    false     -> throw(?EX_UNSUPPORTED_ARROW);
    {_, Func} -> applyArguments(Func, [ castArgumentType(X) || X <- Tail])
  end.

% applies recursively Args to the function Func
applyArguments(Func, Args) -> lists:foldl(fun(E, A) -> A(E) end, Func, Args).

% convert the argument string to the type it says it is (the first character)
castArgumentType(Arg) -> 
  case Arg of
    []        -> throw(?EX_EMPTY_ARGUMENT);
    [T | Str] -> 
      case lists:keyfind(T, 1, ?TYPE_LIST) of
        false     -> throw(?EX_UNSUPPORTED_TYPE);
        {_, Func} -> Func(Str)
      end
  end.
