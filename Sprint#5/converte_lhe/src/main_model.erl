-module(main_model).
-export([main/1]).

%% tries to process the data with the composition
%% receives a list with the input data and the composition as a string
%% returns an Either like tuple: {bool, result or error}
main({DataList, CompStr, ApiKey}) -> 
    try   spend_usage(ApiKey)
    of    ok  -> run_composition({DataList, CompStr})
    catch _:E -> {false, handle_exception(E)}
    end.
    
run_composition({DataList, CompStr}) ->
    CallerPid = self(),

    Pid = spawn_link(fun() -> 
        try
            CompositionFun = arrow:resolve_composition(arrow:parse_composition(CompStr)),
            lists:map(CompositionFun, DataList)
        of R -> CallerPid ! {ok, R}
        catch _:M -> CallerPid ! {handled, handle_exception(M)}
        end
    end),
    
    receive
        {ok, Res}    -> {true, [handle_result(X) || X <- Res]};
        {handled, H} -> {false, H};
        _            -> {false, handle_exception(unexpected)}
    after 5000 ->  exit(Pid, kill), {false, <<"error: timeout - execution took too long">>}
    end.


spend_usage(ApiKey) -> 
    case dao:get_user_remaining_limit(ApiKey) of
        0 -> throw({exception_not_enough_limit});
        _ -> case dao:spend_usage(ApiKey) of
                true -> ok;
                _    -> throw({exception_not_enough_limit})
             end
    end.

handle_result(X) -> handle_result_arr([X]).

handle_result_arr([])             -> <<"">>;
handle_result_arr([Value])        -> <<(handle_result_ele(Value))/binary>>;
handle_result_arr([Value | Tail]) -> <<(handle_result_ele(Value))/binary, "," , (handle_result_arr(Tail))/binary>>;
handle_result_arr(Value)          -> handle_result_ele(Value).

handle_result_ele(Value) when is_binary(Value) -> Value;
handle_result_ele([])                          -> <<"[]">>;
handle_result_ele(Value) when is_list(Value)   -> <<"[", (handle_result_arr(Value))/binary, "]">>;
handle_result_ele(Value) -> <<(list_to_binary(io_lib:format("~w", [Value])))/binary>>.

% returns the response based on received exception
handle_exception(E) -> case E of
    {exception_unsupported_prefix, P}           -> <<"unsupported prefix: ", (list_to_binary(P))/binary>>;
    {exception_unsupported_function, F}         -> <<"unsupported function: ", (list_to_binary(F))/binary>>;
    {exception_convert_float, V}                -> <<"failed convertion to float: ", (list_to_binary(V))/binary>>;
    {exception_convert_integer, V}              -> <<"failed convertion to integer: ", (list_to_binary(V))/binary>>;
    {exception_convert_string, V}               -> <<"failed convertion to string: ", (list_to_binary(V))/binary>>;
    {exception_inexistent_variable_position, V} -> 
        <<"inexistent variable position: ", (list_to_binary(lists:flatten(io_lib:format("~w",[V]))))/binary>>;
    {exception_bad_argument, V} -> 
        <<"failed function application to arguments: ", (list_to_binary(lists:flatten(io_lib:format("~w",[V]))))/binary>>;
    {exception_not_enough_values_on_stack}      -> <<"not enough values on the stack">>;
    {exception_too_many_arguments, V} -> 
        <<"too many arugments applied to function: ", (list_to_binary(lists:flatten(io_lib:format("~w",[V]))))/binary>>;
    {exception_non_closing_composition}       -> <<"non closing composition">>;
    {exception_empty_composition}             -> <<"empty composition">>;
    {exception_not_enough_limit}              -> <<"not enough remaining limit, check if your apikey is correct and/or try again later">>;
        
    {exception_negative_arity, _V}            -> <<"internal error: negative arity">>;
    {exception_not_integer, _V}               -> <<"internal error: not an integer">>;
    {exception_unsupported_type, _V}          -> <<"internal error: unsupported type">>;
    _   -> <<"unexpected error">>
    end.