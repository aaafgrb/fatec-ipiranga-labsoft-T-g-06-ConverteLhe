-module(function_list).
-export([func/1]).

%% the function list
%% returns a tuple with 
%%   the arity (an integer or 'infinity') 
%%   the function (that receives a list)

% conversions

func("toInt")    -> { 1, fun([F]) -> 
                            C = try binary_to_list(F) catch _ -> throw({exception_convert_integer, "malformed string"}) end,
                            case string:to_integer(C) of
                                {N, []} -> N;
                                _       -> throw({exception_convert_integer, C})
                            end
                        end};
func("toFloat")  -> { 1, fun([F]) -> 
                            C = try binary_to_list(F) catch _ -> throw({exception_convert_float, "malformed string"}) end,
                            case 
                                string:to_float(C) of
                                {N, []} -> N;
                                _       -> throw({exception_convert_float, C})
                            end    
                        end};

func("toString") -> { 1, fun([F]) -> list_to_binary(lists:flatten(io_lib:format("~s",[F]))) end};

func("degreesToRadians") -> { 1, fun([F]) -> F * math:pi() / 180 end};
func("radiansToDegrees") -> { 1, fun([F]) -> F * 180 / math:pi() end};

% numbers

func("sum")      -> { 2, fun([F, S]) -> F + S end };
func("subtract") -> { 2, fun([F, S]) -> F - S end };
func("multiply") -> { 2, fun([F, S]) -> F * S end };
func("divide")   -> { 2, fun([F, S]) -> F / S end };
func("sqrt")     -> { 1, fun([F]) -> math:sqrt(F) end };
func("log")      -> { 1, fun([F]) -> math:log(F) end };
func("log10")    -> { 1, fun([F]) -> math:log10(F) end };
func("log2")     -> { 1, fun([F]) -> math:log2(F) end };
func("sin")      -> { 1, fun([F]) -> math:sin(F) end };
func("cos")      -> { 1, fun([F]) -> math:cos(F) end };
func("tan")      -> { 1, fun([F]) -> math:tan(F) end };
func("asin")     -> { 1, fun([F]) -> math:asin(F) end };
func("acos")     -> { 1, fun([F]) -> math:acos(F) end };
func("atan")     -> { 1, fun([F]) -> math:atan(F) end };
func("erf")      -> { 1, fun([F]) -> math:erf(F) end };
func("erfc")     -> { 1, fun([F]) -> math:erfc(F) end };
func("floor")    -> { 1, fun([F]) -> math:floor(F) end };
func("ceil")     -> { 1, fun([F]) -> math:ceil(F) end };
func("pow")      -> { 2, fun([F, S]) -> math:pow(F, S) end };
func("exp")      -> { 1, fun([F]) -> math:exp(F) end };

% lists

func("arr_range")       -> { 2, fun([F, S]) -> try lists:seq(F, S) 
                                               of R -> R
                                               catch _:function_clause -> []
                                               end end };
func("arr_list")        -> { infinity, fun(Arr) -> Arr end };
func("arr_index")       -> { 2, fun([L, I]) -> lists:nth(I, L) end };
func("arr_length")      -> { 1, fun([A]) -> length(A) end };
func("arr_push")        -> { 2, fun([A, E]) -> A ++ [E] end };
func("arr_pop")         -> { 1, fun([A]) -> try lists:droplast(A) 
                                            of R -> R
                                            catch _:function_clause -> []
                                            end end };
func("arr_intersperse") -> { 2, fun([L, D]) -> intersperse(D, L) end};

func("arr_concat")      -> { 2, fun([F, S]) -> F ++ S end };

func("arr_head")        -> { 1, fun([A]) -> [H|_] = A, H end };
func("arr_tail")        -> { 1, fun([A]) -> [_|T] = A, T end };

func("arr_duplicate")   -> { 2, fun([N, E]) -> lists:duplicate(N, E) end };

% functions

func("foldl")    -> { 3, fun([F, A0, S]) -> lists:foldl(fun(E, A) -> F([A, E]) end, A0, S) end };
func("map")      -> { 2, fun([F, S])     -> lists:map(fun(E) -> F([E]) end, S) end };

% strings

func("split")    -> { 2, fun([S, D]) -> binary:split(S, [D]) end };
func("concat")   -> { 2, fun([F, S]) -> <<F/binary, S/binary>> end };


func(F) -> throw({exception_unsupported_function, F}).

% --- helpers ---

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(D, [X | Rest]) -> intersperse_helper(D, [X], Rest).

intersperse_helper(_, Acc, []) -> lists:reverse(Acc);
intersperse_helper(D, Acc, [X | Rest]) -> intersperse_helper(D, [X, D | Acc], Rest).