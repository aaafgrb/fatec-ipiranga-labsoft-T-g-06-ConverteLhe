-module(arrow).
-export([parseComposition/1, resolveComposition/1]).

%% arrow related stuff 
%%   composition parsing and solving
%%   list of usable functions
%%   type parsing
%%   prefix settings
%%   variable tree getter and setter
%%
%% 'Tree' refered down isnt really a tree datatype. its more like a jagged list situation


% parse an arrow composition string to a composition part tuple list
%   expects rpn format
parseComposition(Str) -> 
  L =  lists:flatten(string:replace(Str, "/", [0], all)),
  LL = lists:flatten(string:replace(L, [92, 0], "/", all)),
  lists:map(fun parseElement/1, string:tokens(LL, [0])).

% receives a string containing a single part with prefix and value
% returns a tuple with an atom (representing the type) and the value (converted to the type)
parseElement([Prefix | Value]) -> P = prefixToType(Prefix), {P, convertToType(P, Value)};
parseElement(_) -> [].

% returns the atom corresponding to each character used as a prefix
prefixToType(36)  -> function;    % $
prefixToType(35)  -> funcApply;   % #
prefixToType(58)  -> store;       % :
prefixToType(120) -> variable;    % x
prefixToType(115) -> string;      % s
prefixToType(102) -> float;       % f
prefixToType(105) -> integer;     % i
prefixToType(T) -> throw({exception_unsupported_prefix, [T]}).

% converts a value string to the corresponding atom value
convertToType(function, Value) -> Value;
convertToType(string, Value)   -> Value;
convertToType(variable, Value) -> try   string:to_integer(Value) 
                                  of    { V, [] } when V > 0 -> [ X - 48 || X <- Value ];
                                        _                    -> throw({exception_convert_variable, Value})
                                  catch error:_              -> throw({exception_convert_variable, Value})
                                  end;
convertToType(store, Value)    -> try   string:to_integer(Value) 
                                  of    { V, [] }  when V > 0 -> [ X - 48 || X <- Value ];
                                        _                     -> throw({exception_convert_store, Value})
                                  catch error:_               -> throw({exception_convert_store, Value})
                                  end;
convertToType(float, Value)    -> try   string:to_float(Value) 
                                  of    { Result, [] } -> Result;
                                        _              -> throw({exception_convert_float, Value})
                                  catch error:_        -> throw({exception_convert_float, Value})
                                  end;
convertToType(integer, Value)  -> try   string:to_integer(Value) 
                                  of    { Result, [] } -> Result;
                                        _              -> throw({exception_convert_integer, Value})
                                  catch error:_        -> throw({exception_convert_integer, Value})
                                  end;
convertToType(funcApply, Value)-> try   string:to_integer(Value) 
                                  of    { Result, [] } -> Result;
                                        _              -> throw({exception_convert_integer, Value})
                                  catch error:_        -> throw({exception_convert_integer, Value})
                                  end;
convertToType(T, _) -> throw({exception_unsupported_type, T}).

% --------------------------------------------------------------------------------

% receives a list of tuples of type and value (the thing that parseComposition returns)
% returns a function that receives a single parameter and uses it to solve the composition
resolveComposition(Composition) -> fun(X) -> resolveComposition(Composition, [X], []) end.

resolveComposition([{string, Value}   |Tail], Tree, Stack) -> resolveComposition(Tail, Tree, [Value|Stack]);
resolveComposition([{float, Value}    |Tail], Tree, Stack) -> resolveComposition(Tail, Tree, [Value|Stack]);
resolveComposition([{integer, Value}  |Tail], Tree, Stack) -> resolveComposition(Tail, Tree, [Value|Stack]);

% ?remove or not remove the value from the stack when storing it on the tree?
resolveComposition([{store, Value}    |Tail], Tree, [S | Stack]) -> 
  resolveComposition(Tail, setTreeVariable(Tree, Value, S), [Stack]);

resolveComposition([{variable, Value} |Tail], Tree, Stack) -> resolveComposition(Tail, Tree, [{variable, Value}|Stack]);
resolveComposition([{function, Value} |Tail], Tree, Stack) -> 
  resolveComposition(Tail, Tree, [handleFuncArity(function_list:func(Value))|Stack]);

resolveComposition([{funcApply, Value} |Tail], Tree, [F|Stack]) -> 
  resolveComposition(Tail, Tree, applyFunc(F, Tree, Stack, Value));

resolveComposition([], T, S) -> {T, S}.

getNFromStack(N, Stack) when is_integer(N) -> 
  try lists:split(N, Stack)
    of    R -> R
    catch error:badarg -> throw({exception_not_enough_values_on_stack, Stack})
  end;
getNFromStack(N, _) -> throw({exception_not_integer, N}).

% applies ArgCount arguments to Func
applyFunc(Func, Tree, Stack, ArgCount) -> 
  {S, Rest} = getNFromStack(ArgCount, Stack),
  Params = handleParams(S, Tree),
  try Func(Params) of
    Res -> [Res|Rest]
  catch
    error:badarith -> throw({exception_bad_argument, Params})
  end.
  

% handles function application parameters
%   if its a variable gets its value from the tree
handleParams(Params, Tree) -> 
  lists:map(fun(E) -> case E of
      {variable, V} -> getTreeVariable(Tree, V);
      V -> V
    end
  end, Params).

% gets the value of the corresponding position on the tree
getTreeVariable(Tree, Pos) -> 
  try getTreeVariableLoop(Tree, Pos)
  of V -> V
  catch
    error:function_clause -> throw({ exception_inexistent_variable_position, Pos })
  end.
% the position is an list of numbers from 1 to 9 
%   therefore thats the maximum number of 'branches' a node can have
getTreeVariableLoop(Tree, [H|T]) -> getTreeVariableLoop(lists:nth(H, Tree), T);
getTreeVariableLoop(Tree, []) -> Tree.

% returns a tree with the corresponding position substituted by the Value
setTreeVariable(Tree, Pos, Value) -> 
  try setTreeVariableLoop(Tree, Pos, Value)
  of V -> V
  catch
    error:function_clause -> throw({ exception_inexistent_variable_position, Pos });
    error:badarg -> throw({ exception_inexistent_variable_position, Pos })
  end.
%   isnt using LCO :(
setTreeVariableLoop(Tree, [H|T], Value) -> 
  {I, [_|II]} = lists:split(H - 1, Tree),
  I ++ [setTreeVariableLoop(lists:nth(H, Tree), T, Value) | II];
setTreeVariableLoop(_, [], Value) -> Value.

handleFuncArity({infinity, F}) -> F;
handleFuncArity({N, F}) -> curry(N, F).

curry(Arity, Func) -> curry(Arity, Func, [], []).

curry(0     , Func , Acc , [])                           -> Func(lists:reverse(Acc));
curry(Arity , Func , Acc , [ArgH | ArgT]) when Arity > 0 -> curry(Arity - 1, Func, [ArgH | Acc], ArgT);
curry(Arity , Func , Acc , [])                           -> fun(Args) -> curry(Arity, Func, Acc, Args) end;
curry(0     , _Func, _Acc, Args)                         -> throw({exception_too_many_arguments, Args});
curry(Arity, _Func, _Acc, _Args)                         -> throw({exception_negative_arity, Arity}).
