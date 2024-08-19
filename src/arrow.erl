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
prefixToType(36)  -> function; % $
prefixToType(58)  -> store;    % :
prefixToType(120) -> variable; % x
prefixToType(115) -> string;   % s
prefixToType(102) -> float;    % f
prefixToType(105) -> integer;  % i
prefixToType(T) -> throw({exception_unsupported_prefix, [T]}).

% converts a value string to the corresponding atom value
convertToType(function, Value) -> Value;
convertToType(string, Value)   -> Value;
convertToType(variable, Value) -> try   string:to_integer(Value) 
                                  of    { _, [] } -> [ X - 48 || X <- Value ];
                                        _              -> throw({exception_convert_variable, Value})
                                  catch error:_        -> throw({exception_convert_variable, Value})
                                  end;
convertToType(store, Value)    -> try   string:to_integer(Value) 
                                  of    { _, [] } -> [ X - 48 || X <- Value ];
                                        _              -> throw({exception_convert_store, Value})
                                  catch error:_        -> throw({exception_convert_store, Value})
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

resolveComposition([{function, Value} |Tail], Tree, Stack) -> resolveComposition(Tail, Tree, applyFunc(Value, Tree, Stack));

resolveComposition([], T, S) -> {T, S}.

% searches for the function Name on the function list and tries to apply it to the values of the Stack
%   uses values from the tree in case there are any 'variable' types
applyFunc(Name, Tree, Stack) -> 
  {Func, Arity} = funcList(Name),
  {S, Rest} = try lists:split(Arity, Stack)
                of    R -> R
                catch error:badarg -> throw({exception_not_enough_parameters, Name})
              end,
  Params =    lists:map(fun(E) -> case E of
                  {variable, V} -> getTreeVariable(Tree, V);
                  V -> V
                end
              end, S),
  [Func(Params)|Rest].

% gets the value of the corresponding position on the tree
%   the position is an list of numbers from 1 to 9 
%     therefore thats the maximum number of 'branches' a node can have
getTreeVariable(Tree, [H|T]) -> getTreeVariable(lists:nth(H, Tree), T);
getTreeVariable(Tree, []) -> Tree.

% returns a tree with the corresponding position substituted by the Value
%   isnt using LCO :(
setTreeVariable(Tree, [H|T], Value) -> 
  {I, [_|II]} = lists:split(H - 1, Tree),
  I ++ [setTreeVariable(lists:nth(H, Tree), T, Value) | II];
setTreeVariable(_, [], Value) -> Value.

% the function list
% returns a tuple with the function (that receives a list) and its arity (the number of parameters)
funcList("sum")      -> { fun([F, S]) -> F + S end, 2 };
funcList("multiply") -> { fun([F, S]) -> F * S end, 2 };
funcList("divide")   -> { fun([F, S]) -> F / S end, 2 };
funcList("clone")    -> { fun([N, F]) -> lists:duplicate(N, F) end, 2 };
funcList("concat")   -> { fun([F, S]) -> F ++ S end, 2 };
funcList(F) -> throw({exception_unsupported_function, F}).