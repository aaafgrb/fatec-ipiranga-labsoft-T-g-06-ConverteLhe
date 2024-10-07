-module(function_list).
-export([func/1]).

%% the function list
%% returns a tuple with 
%%   the arity (an integer or 'infinity') 
%%   the function (that receives a list)

func("toInt")    -> { 1, fun([F]) -> 
                            case string:to_integer(F) of
                                {N, []} -> N;
                                _       -> throw({exception_convert_integer, F})
                            end
                        end};
func("toFloat")  -> { 1, fun([F]) -> 
                            case string:to_float(F) of
                                {N, []} -> N;
                                _       -> throw({exception_convert_float, F})
                            end    
                        end};

func("toString") -> { 1, fun([F]) -> lists:flatten(io_lib:format("~p",[F])) end};

func("sum")      -> { 2, fun([F, S]) -> F + S end };
func("multiply") -> { 2, fun([F, S]) -> F * S end };
func("divide")   -> { 2, fun([F, S]) -> F / S end };
func("sqrt")     -> { 1, fun([F]) -> math:sqrt(F) end };
func("log")      -> { 1, fun([F]) -> math:log(F) end };
func("log10")    -> { 1, fun([F]) -> math:log10(F) end };
func("log2")     -> { 1, fun([F]) -> math:log2(F) end };

func("clone")    -> { 2, fun([N, F]) -> lists:duplicate(N, F) end };

func("range")    -> { 2, fun([F, S]) -> try lists:seq(F, S) 
                                        of R -> R
                                        catch _:function_clause -> []
                                        end end };

func("list")     -> { infinity, fun(Arr) -> Arr end };
func("length")   -> { 1, fun([A]) -> length(A) end };
func("push")     -> { 2, fun([A, E]) -> A ++ [E] end };
func("pop")      -> { 1, fun([A]) -> try lists:droplast(A) 
                                     of R -> R
                                     catch _:function_clause -> []
                                     end end };

func("concat")   -> { 2, fun([F, S]) -> F ++ S end };

func("head")     -> { 1, fun([A]) -> [H|_] = A, H end };
func("tail")     -> { 1, fun([A]) -> [_|T] = A, T end };

func("duplicate")-> { 2, fun([N, E]) -> lists:duplicate(N, E) end };

func("foldl")    -> { 3, fun([F, A0, S]) -> lists:foldl(fun(E, A) -> F([A, E]) end, A0, S) end };
func("map")      -> { 2, fun([F, S])     -> lists:map(fun(E) -> F([E]) end, S) end };

func(F) -> throw({exception_unsupported_function, F}).