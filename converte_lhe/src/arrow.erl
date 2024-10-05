-module(arrow).
-export([parse_composition/1, resolve_composition/1]).

%% arrow related stuff 
%%   composition parsing and solving
%%   list of usable functions
%%   type parsing
%%   prefix settings
%%


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
prefix_to_type(35)  -> inf_param;   % #
prefix_to_type(36)  -> function;    % $
prefix_to_type(60)  -> comp_start;  % <
prefix_to_type(62)  -> comp_end;    % >
prefix_to_type(64)  -> fin_param;   % @
prefix_to_type(120) -> variable;    % x
prefix_to_type(115) -> string;      % s
prefix_to_type(102) -> float;       % f
prefix_to_type(105) -> integer;     % i
prefix_to_type(T) -> throw({exception_unsupported_prefix, [T]}).

% converts a value string to the corresponding atom value
convert_to_type(function, Value)   -> Value;
convert_to_type(string, Value)     -> Value;
convert_to_type(comp_start, Value) -> Value; % the value isnt used for anything currently
convert_to_type(comp_end, Value)   -> Value; % the value isnt used for anything currently
convert_to_type(variable, Value)   ->  try   string:to_integer(Value) 
                                       of    { Result, [] } -> Result;
                                             _              -> throw({exception_convert_integer, Value})
                                       catch error:_        -> throw({exception_convert_integer, Value})
                                       end;
convert_to_type(float, Value)      ->  try   string:to_float(Value) 
                                       of    { Result, [] } -> Result;
                                             _              -> throw({exception_convert_float, Value})
                                       catch error:_        -> throw({exception_convert_float, Value})
                                       end;
convert_to_type(integer, Value)    ->  try   string:to_integer(Value) 
                                       of    { Result, [] } -> Result;
                                             _              -> throw({exception_convert_integer, Value})
                                       catch error:_        -> throw({exception_convert_integer, Value})
                                       end;
convert_to_type(inf_param, Value)  ->  try   string:to_integer(Value) 
                                       of    { Result, [] } -> Result;
                                             _              -> throw({exception_convert_integer, Value})
                                       catch error:_        -> throw({exception_convert_integer, Value})
                                       end;
convert_to_type(fin_param, Value)  ->  try   string:to_integer(Value) 
                                       of    { V, [] } when V > 0 -> [ X - 48 || X <- Value ];
                                             _              -> throw({exception_convert_integer, Value})
                                       catch error:_        -> throw({exception_convert_integer, Value})
                                       end;
convert_to_type(T, _) -> throw({exception_unsupported_type, T}).

% --------------------------------------------------------------------------------

% receives a list of tuples of type and value (the thing that parseComposition returns)
% returns a function that receives a single parameter and uses it to solve the composition
resolve_composition(Composition) -> fun(X) -> resolve_composition(Composition, X, []) end.

resolve_composition([{string, Value}     |Tail], Param, Stack) -> resolve_composition(Tail, Param, [Value|Stack]);
resolve_composition([{float, Value}      |Tail], Param, Stack) -> resolve_composition(Tail, Param, [Value|Stack]);
resolve_composition([{integer, Value}    |Tail], Param, Stack) -> resolve_composition(Tail, Param, [Value|Stack]);

resolve_composition([{comp_start, Value} |Tail], Param, Stack) -> 
    {Rest, Comp} = get_inner_composition([{comp_start, Value}|Tail]),
    resolve_composition([Comp|Rest], Param, Stack);

% ignoring composition end with no start
resolve_composition([{comp_end, _Value}  |Tail], Param, Stack) -> resolve_composition(Tail, Param, Stack);

resolve_composition([{variable, Value}   |Tail], Param, Stack) -> resolve_composition(Tail, Param, [{variable, Value}|Stack]);
resolve_composition([{function, Value}   |Tail], Param, Stack) -> resolve_composition(Tail, Param, [handle_func_arity(function_list:func(Value))|Stack]);
resolve_composition([{composition, Value}|Tail], Param, Stack) -> resolve_composition(Tail, Param, [handle_func_arity(Value)|Stack]);

resolve_composition([{inf_param, Value}  |Tail], Param, [F|Stack]) -> resolve_composition(Tail, Param, apply_inf_param(F, Param, Stack, Value));
resolve_composition([{fin_param, Value}  |Tail], Param, [F|Stack]) -> resolve_composition(Tail, Param, apply_fin_param(F, Param, Stack, Value));
resolve_composition([], _P, [])    -> throw({exception_empty_composition});
resolve_composition([], _P, [S|_]) -> S.


get_inner_composition([{comp_start,_Value} |Tail]) -> get_inner_composition(Tail, [], 0, 1).
get_inner_composition([{comp_end  , Value} |Tail], Stack, Arity, Deph) -> get_inner_composition(Tail, [{comp_end  , Value}|Stack], Arity, Deph - 1);
get_inner_composition([{comp_start, Value} |Tail], Stack, Arity, Deph) -> get_inner_composition(Tail, [{comp_start, Value}|Stack], Arity, Deph + 1);
get_inner_composition([{variable, Value}   |Tail], Stack, Arity, 1) when Arity < Value -> get_inner_composition(Tail, [{variable, Value}|Stack], Value, 1);
get_inner_composition(Stack0, [{comp_end,_Value}|Tail], Arity, 0) -> {Stack0, {composition, {Arity, resolve_composition(lists:reverse(Tail))}}};
get_inner_composition([Head|Tail], Stack, Arity, Deph)            -> get_inner_composition(Tail, [Head|Stack], Arity, Deph);
get_inner_composition([], _Stack, _Arity, _Deph)                  -> throw({exception_non_closing_composition}).


handle_func_arity({infinity, F}) -> inf_func(F);
handle_func_arity({A, F})        -> fin_func(A, F).

apply_inf_param(Func, Param, Stack, Count) -> 
    {Args, Rest} = handle_infinite_params(Param, Stack, Count),
    apply_func(Func, Args, Rest).

apply_fin_param(Func, Param, Stack, Indexes) ->
    {Args, Rest} = handle_finite_params(Param, Stack, Indexes),
    apply_func(Func, Args, Rest).

apply_func(Func, Args, Rest) ->
    try   Func(Args) 
    of    Res -> [Res|Rest]
    catch error:badarith -> throw({exception_bad_argument, Args})
    end.

handle_infinite_params(Param, Stack, Count) -> handle_infinite_params(Param, Stack, Count, []).
handle_infinite_params(_    , []           , Count, _  ) when Count > 0 -> throw({exception_not_enough_values_on_stack});
handle_infinite_params(Param, [SHead|STail], Count, Acc) when Count > 0 -> handle_infinite_params(Param, STail, Count - 1, [handle_param_type(Param, SHead)|Acc]);
handle_infinite_params(_    , Stack        , 0    , Acc) -> {lists:reverse(Acc), Stack};
handle_infinite_params(_    , _            , _    , _  ) -> throw({exception_negative_arity}).

handle_finite_params(Param, Stack, Indexes) -> handle_finite_params(Param, Stack, Indexes, []).
handle_finite_params(Param, [SHead|STail], [Index|ITail], Acc) -> handle_finite_params(Param, STail, ITail, [{indexed, Index, handle_param_type(Param, SHead)}|Acc]);
handle_finite_params(_    , Stack                , []   , Acc) -> {lists:reverse(Acc), Stack};
handle_finite_params(_    , []                   , [_|_], _  ) -> throw({exception_not_enough_values_on_stack}).

handle_param_type(Param , {variable, V}) -> try   lists:nth(V, Param)
                                            catch _:_ -> throw({exception_inexistent_variable_position, V})
                                            end;
handle_param_type(_Param, V) -> V.


inf_func(Func) -> fun(Args) -> inf_func(Func, Args) end.
inf_func(Func, [{indexed, Index, Value} | Tail]) -> 
    inf_func(Func, 
        [V || {indexed, _I, V} <- lists:sort(fun({indexed, IA, _A}, {indexed, IB, _B} ) -> IA =< IB end, [{indexed, Index, Value} | Tail])]
    );
inf_func(Func, Args) -> Func(Args).


fin_func(Arity, Func) -> fin_func(lists:duplicate(Arity, null), Func, []).
fin_func(Args, Func, []) ->
    case lists:filter(fun(X) -> X =:= null end, Args) of
        []  -> Func(Args);
        _   -> fun(Params) -> fin_func(Args, Func, Params) end
    end;

fin_func(Args , Func , [{indexed, Index, Value} | Tail]) -> 
    try   lists:split(Index - 1, Args) 
    of    {I, [_|II]} -> fin_func(I ++ [Value | II], Func, Tail);
          _   -> throw({exception_too_many_arguments, Index}) 
    catch _:_ -> throw({exception_too_many_arguments, Index})
    end;

fin_func(Args , Func , [Value | Tail]) -> 
    try   lists:splitwith(fun(A) -> A =/= null end, Args)
    of    {_  , []}         -> throw({exception_too_many_arguments, [Value | Tail]});
          {Bef, [null|Aft]} -> fin_func(Bef ++ [Value | Aft], Func, Tail);
          _   -> throw({exception_too_many_arguments, [Value | Tail]}) 
    catch _:_ -> throw({exception_too_many_arguments, [Value | Tail]})
    end;

fin_func(_Args, _Func, Input) -> throw({exception_incorrect_parameter, Input}).
