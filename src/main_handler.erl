-module(main_handler).
-behavior(cowboy_handler).

-export([init/2]).

% ↓↓↓ next ↓↓↓
% exceptions
%   invalid argument type
%   store doenst exist
%   variable doesnt exist
% github actions testing

%--------------------------------------------------------------

%% currently
%% only receives url parameter "param" with the composition
%% and json containing { "processThis" : string with input values }
%%   it will not proceed if its not a string
%%
%% only accepts basic encoding rn

% receives the http request
init(Req0, State) ->
    Content = 
        try
            erlang:display("---------------------------------------"),
            Data = getInputData(Req0),
            erlang:display(Data),
            CompositionFun = getInputComposition(Req0), 
            Result = lists:map(CompositionFun, Data),
            erlang:display(Result),
            Result
        of
            Res -> #{<<"result">> => [erlang:list_to_binary(lists:flatten(io_lib:format("~p",[X]))) || {_, [X|_]} <- Res], 
                     <<"error">>  => <<"">>}
        catch
            _:E -> #{<<"result">> => <<"">>, <<"error">> => handleException(E)}
        end,
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Content), Req0),
    {ok, Req, State}.


%-------------------------------------------------------------------------------

% returns the response based on received exception
handleException(E) -> case E of
    {exception_incorrect_json_format}         -> <<"incorrect json format">>;
    {exception_incorrect_json_header, Header} -> <<"incorrect json header: ", Header/binary>>;
    {exception_unsupported_prefix, P}         -> <<"unsupported prefix: ", (list_to_binary(P))/binary>>;
    {exception_unsupported_function, F}       -> <<"unsupported function: ", (list_to_binary(F))/binary>>;
    {exception_convert_float, V}              -> <<"failed convertion to float: ", (list_to_binary(V))/binary>>;
    {exception_convert_integer, V}            -> <<"failed convertion to integer: ", (list_to_binary(V))/binary>>;
    {exception_convert_store, V}              -> <<"failed convertion to store: ", (list_to_binary(V))/binary>>;
    {exception_convert_variable, V}           -> <<"failed convertion to variable: ", (list_to_binary(V))/binary>>;
    {exception_not_enough_values_on_stack, S} -> 
        <<"not enough values on the stack: ", (list_to_binary(lists:flatten(io_lib:format("~p",[S]))))/binary>>;
    {exception_too_many_arguments, V} -> 
        <<"too many arugments applied to function: ", (list_to_binary(lists:flatten(io_lib:format("~p",[V]))))/binary>>;
        
    {exception_negative_arity, _V}            -> <<"internal error: negative arity">>;
    {exception_not_integer, _V}               -> <<"internal error: not an integer">>;
    {exception_unsupported_type, _V}          -> <<"internal error: unsupported type">>;
    _   -> <<"unexpected error">>
    end.
%-------------------------------------------------------------------------------

% gets the input data from the request json
getInputData(Req0) -> 
    {ok, JsonBin, _} = read_body(Req0, <<>>),
    Json = jsx:decode(JsonBin),
    case maps:find(<<"processThis">>, Json) of 
        {ok, Value} when is_binary(Value) -> [erlang:binary_to_list(Value)];
        {ok, _Value}                      -> throw({exception_incorrect_json_header, <<"processThis">>});
        error                             -> throw({exception_incorrect_json_format})
    end.

% gets the url parameter "param"
getInputComposition(Req0) ->
    case cowboy_req:match_qs([{param,[], <<>>}], Req0) of
        #{param := V} -> arrow:resolveComposition(arrow:parseComposition(binary_to_list(V)));
        <<>> -> <<>>
    end.

%-------------------------------------------------

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.