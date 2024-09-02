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

func("concat")   -> { 2, fun([F, S]) -> F ++ S end };

func("clone")    -> { 2, fun([N, F]) -> lists:duplicate(N, F) end };

func("list")     -> { infinity, fun(Arr) -> Arr end } ;

func("foldl")    -> { 3, fun([F, A0, S]) -> lists:foldl(fun(E, A) -> F([A, E]) end, A0, S) end };
func("map")      -> { 2, fun([F, S])     -> lists:map(fun(E) -> F([E]) end, S) end };

func(F) -> throw({exception_unsupported_function, F}).