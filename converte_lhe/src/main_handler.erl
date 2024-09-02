-module(main_handler).
-behavior(cowboy_handler).

-export([init/2]).

%% currently
%% only receives url parameter "param" with the composition
%% and json containing { "processThis" : string with input values }
%%   it will not proceed if its not a string
%%
%% only accepts basic encoding rn

% receives the http request
init(Req0, State) ->
    R = try main_model:main(getInputData(Req0), getInputComposition(Req0))
        of V -> V
        catch
            {exception_incorrect_json_format}         -> {false, <<"incorrect json format">>};
            {exception_incorrect_json_header, Header} -> {false, <<"incorrect json header: ", Header/binary>>};
            _                                         -> {false, <<"unexpected error">>}
        end,

    C = case R of
            {true, Res}  -> #{<<"result">> => Res, <<"error">> => <<"">>};
            {false, Err} -> #{<<"result">> => <<"">>, <<"error">> => Err};
            _            -> #{<<"result">> => <<"">>, <<"error">> => <<"unexpected error">>}
        end,
    
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(C), Req0),
    {ok, Req, State}.
    
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
        #{param := V} -> binary_to_list(V);
        <<>> -> <<>>
    end.

%-------------------------------------------------

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.