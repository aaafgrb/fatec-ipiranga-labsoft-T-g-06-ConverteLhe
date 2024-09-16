-module(arrow).
-export([parse_composition/1, resolve_composition/1]).

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
parse_composition(Str) -> 
    L =  lists:flatten(string:replace(Str, "/", [0], all)),
    LL = lists:flatten(string:replace(L, [92, 0], "/", all)),
    lists:map(fun parse_element/1, string:tokens(LL, [0])).

% receives a string containing a single part with prefix and value
% returns a tuple with an atom (representing the type) and the value (converted to the type)
parse_element([Prefix | Value]) -> P = prefix_to_type(Prefix), {P, convert_to_type(P, Value)};
parse_element(_) -> [].

% returns the atom corresponding to each character used as a prefix
prefix_to_type(36)  -> function;    % $
prefix_to_type(35)  -> funcApply;   % #
prefix_to_type(58)  -> store;       % :
prefix_to_type(120) -> variable;    % x
prefix_to_type(115) -> string;      % s
prefix_to_type(102) -> float;       % f
prefix_to_type(105) -> integer;     % i
prefix_to_type(T) -> throw({exception_unsupported_prefix, [T]}).

% converts a value string to the corresponding atom value
convert_to_type(function, Value) -> Value;
convert_to_type(string, Value)   -> Value;
convert_to_type(variable, Value) -> try   string:to_integer(Value) 
                                    of    { V, [] } when V > 0 -> [ X - 48 || X <- Value ];
                                            _                    -> throw({exception_convert_variable, Value})
                                    catch error:_              -> throw({exception_convert_variable, Value})
                                    end;
convert_to_type(store, Value)    -> try   string:to_integer(Value) 
                                    of    { V, [] }  when V > 0 -> [ X - 48 || X <- Value ];
                                            _                     -> throw({exception_convert_store, Value})
                                    catch error:_               -> throw({exception_convert_store, Value})
                                    end;
convert_to_type(float, Value)    -> try   string:to_float(Value) 
                                    of    { Result, [] } -> Result;
                                            _              -> throw({exception_convert_float, Value})
                                    catch error:_        -> throw({exception_convert_float, Value})
                                    end;
convert_to_type(integer, Value)  -> try   string:to_integer(Value) 
                                    of    { Result, [] } -> Result;
                                            _              -> throw({exception_convert_integer, Value})
                                    catch error:_        -> throw({exception_convert_integer, Value})
                                    end;
convert_to_type(funcApply, Value)-> try   string:to_integer(Value) 
                                    of    { Result, [] } -> Result;
                                            _              -> throw({exception_convert_integer, Value})
                                    catch error:_        -> throw({exception_convert_integer, Value})
                                    end;
convert_to_type(T, _) -> throw({exception_unsupported_type, T}).

% --------------------------------------------------------------------------------

% receives a list of tuples of type and value (the thing that parseComposition returns)
% returns a function that receives a single parameter and uses it to solve the composition
resolve_composition(Composition) -> fun(X) -> resolve_composition(Composition, [X], []) end.

resolve_composition([{string, Value}   |Tail], Tree, Stack) -> resolve_composition(Tail, Tree, [Value|Stack]);
resolve_composition([{float, Value}    |Tail], Tree, Stack) -> resolve_composition(Tail, Tree, [Value|Stack]);
resolve_composition([{integer, Value}  |Tail], Tree, Stack) -> resolve_composition(Tail, Tree, [Value|Stack]);

% ?remove or not remove the value from the stack when storing it on the tree?
resolve_composition([{store, Value}    |Tail], Tree, [S | Stack]) -> 
    resolve_composition(Tail, set_tree_variable(Tree, Value, S), [Stack]);

resolve_composition([{variable, Value} |Tail], Tree, Stack) -> resolve_composition(Tail, Tree, [{variable, Value}|Stack]);
resolve_composition([{function, Value} |Tail], Tree, Stack) -> 
    resolve_composition(Tail, Tree, [handle_func_arity(function_list:func(Value))|Stack]);

resolve_composition([{funcApply, Value} |Tail], Tree, [F|Stack]) -> 
    resolve_composition(Tail, Tree, apply_func(F, Tree, Stack, Value));

resolve_composition([], T, S) -> {T, S}.

get_n_from_stack(N, Stack) when is_integer(N) -> 
    try lists:split(N, Stack)
        of    R -> R
        catch error:badarg -> throw({exception_not_enough_values_on_stack, Stack})
    end;
get_n_from_stack(N, _) -> throw({exception_not_integer, N}).

% applies ArgCount arguments to Func
apply_func(Func, Tree, Stack, ArgCount) -> 
    {S, Rest} = get_n_from_stack(ArgCount, Stack),
    Params = handle_params(S, Tree),
    try Func(Params) of
        Res -> [Res|Rest]
    catch
        error:badarith -> throw({exception_bad_argument, Params})
    end.
  

% handles function application parameters
%   if its a variable gets its value from the tree
handle_params(Params, Tree) -> 
    lists:map(fun(E) -> case E of
        {variable, V} -> get_tree_variable(Tree, V);
        V -> V
        end
    end, Params).

% gets the value of the corresponding position on the tree
get_tree_variable(Tree, Pos) -> 
    try get_tree_variable_loop(Tree, Pos)
    of V -> V
    catch
        error:function_clause -> throw({ exception_inexistent_variable_position, Pos })
    end.
% the position is an list of numbers from 1 to 9 
%   therefore thats the maximum number of 'branches' a node can have
get_tree_variable_loop(Tree, [H|T]) -> get_tree_variable_loop(lists:nth(H, Tree), T);
get_tree_variable_loop(Tree, []) -> Tree.

% returns a tree with the corresponding position substituted by the Value
set_tree_variable(Tree, Pos, Value) -> 
    try set_tree_variable_loop(Tree, Pos, Value)
    of V -> V
    catch
        error:function_clause -> throw({ exception_inexistent_variable_position, Pos });
        error:badarg -> throw({ exception_inexistent_variable_position, Pos })
    end.
%   isnt using LCO :(
set_tree_variable_loop(Tree, [H|T], Value) -> 
    {I, [_|II]} = lists:split(H - 1, Tree),
    I ++ [set_tree_variable_loop(lists:nth(H, Tree), T, Value) | II];
set_tree_variable_loop(_, [], Value) -> Value.

handle_func_arity({infinity, F}) -> F;
handle_func_arity({N, F}) -> curry(N, F).

curry(Arity, Func) -> curry(Arity, Func, [], []).

curry(0     , Func , Acc , [])                           -> Func(lists:reverse(Acc));
curry(Arity , Func , Acc , [ArgH | ArgT]) when Arity > 0 -> curry(Arity - 1, Func, [ArgH | Acc], ArgT);
curry(Arity , Func , Acc , [])                           -> fun(Args) -> curry(Arity, Func, Acc, Args) end;
curry(0     , _Func, _Acc, Args)                         -> throw({exception_too_many_arguments, Args});
curry(Arity, _Func, _Acc, _Args)                         -> throw({exception_negative_arity, Arity}).
