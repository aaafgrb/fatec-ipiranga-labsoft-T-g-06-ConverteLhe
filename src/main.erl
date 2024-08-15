-module(main).
-import(arrow, [parseArrow/1]).
-export([start/0]).

start() -> 
  
  try parseArrow("sum") of
    {_, Func} -> Func()
  catch
    error:Error -> {error, caught, Error};
    exit:Exit   -> {exit, caught, Exit};
    throw:Throw -> {throw, caught, Throw};
    _:_         -> {unknown, caught}
  end.
  %lists:foldl(fun(Elem, Acc) -> Elem(Acc) end, 2, [sum(3), sum(5)] ).



