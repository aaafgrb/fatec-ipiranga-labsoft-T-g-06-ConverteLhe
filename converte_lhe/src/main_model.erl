-module(main_model).
-export([main/2]).

%% tries to process the data with the composition
%% receives a list with the input data and the composition as a string
%% returns an Either like tuple: {bool, result or error}
main(DataList, CompStr) -> 
    try
        CompositionFun = arrow:resolveComposition(arrow:parseComposition(CompStr)),
        Result = lists:map(CompositionFun, DataList),
        erlang:display(Result),
        Result
    of
        Res -> {true, [handleResult(X) || {_, [X|_]} <- Res]}
    catch
        _:E -> {false, handleException(E)}
    end.


%converts the result a string then to binary
handleResult(X) -> erlang:list_to_binary(lists:flatten(io_lib:format("~p",[X]))).

% returns the response based on received exception
handleException(E) -> case E of
    {exception_unsupported_prefix, P}           -> <<"unsupported prefix: ", (list_to_binary(P))/binary>>;
    {exception_unsupported_function, F}         -> <<"unsupported function: ", (list_to_binary(F))/binary>>;
    {exception_convert_float, V}                -> <<"failed convertion to float: ", (list_to_binary(V))/binary>>;
    {exception_convert_integer, V}              -> <<"failed convertion to integer: ", (list_to_binary(V))/binary>>;
    {exception_convert_store, V}                -> <<"failed convertion to store: ", (list_to_binary(V))/binary>>;
    {exception_convert_variable, V}             -> <<"failed convertion to variable: ", (list_to_binary(V))/binary>>;
    {exception_inexistent_variable_position, V} -> 
        <<"inexistent variable position: ", (list_to_binary(lists:flatten(io_lib:format("~p",[V]))))/binary>>;
    {exception_bad_argument, V} -> 
        <<"failed function application to arguments: ", (list_to_binary(lists:flatten(io_lib:format("~p",[V]))))/binary>>;
    {exception_not_enough_values_on_stack, S}   -> 
        <<"not enough values on the stack: ", (list_to_binary(lists:flatten(io_lib:format("~p",[S]))))/binary>>;
    {exception_too_many_arguments, V} -> 
        <<"too many arugments applied to function: ", (list_to_binary(lists:flatten(io_lib:format("~p",[V]))))/binary>>;
        
    {exception_negative_arity, _V}            -> <<"internal error: negative arity">>;
    {exception_not_integer, _V}               -> <<"internal error: not an integer">>;
    {exception_unsupported_type, _V}          -> <<"internal error: unsupported type">>;
    _   -> <<"unexpected error">>
    end.