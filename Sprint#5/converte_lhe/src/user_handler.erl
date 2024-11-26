-module(user_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Content = try 
        D = get_input_data(Req0),
        user_model:handle_req(D)
    of
        false -> #{<<"value">> => <<"">>,            <<"error">> => <<"">>};
        R     -> #{<<"value">> => list_to_binary(R), <<"error">> => <<"">>}
    catch
        {exception_unsupported_req, R}    -> #{<<"value">> => <<"">>, <<"error">> => <<"unsupported requisition: ", R/binary>>};
        {exception_incorrect_json_format} -> #{<<"value">> => <<"">>, <<"error">> => <<"incorrect json format">>};
        {exception_notok_request, N}      -> #{<<"value">> => <<"">>, 
                                               <<"error">> => list_to_binary("request failed: " ++ (integer_to_list(N)))};
        _:_                               -> #{<<"value">> => <<"">>, <<"error">> => <<"unexpected error">>}
    end,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Content), Req0),
    {ok, Req, State}.


% gets the input data from the request json
get_input_data(Req0) -> 
    try
        {ok, Json, _} = req_util:read_body(Req0, <<>>),
        Req = case maps:find(<<"req">>, Json) of
            {ok, R} -> binary_to_list(R);
            error   -> false
        end,
        Email = case maps:find(<<"email">>, Json) of
            {ok, E} -> binary_to_list(E);
            error   -> false
        end,
        Key = case maps:find(<<"key">>, Json) of
            {ok, K} -> binary_to_list(K);
            error   -> false
        end,
        Pass = case maps:find(<<"pass">>, Json) of
            {ok, P} -> binary_to_list(P);
            error   -> false
        end,
        {Req, Email, Key, Pass}
    of 
        V -> V
    catch
        error:badarg -> throw({exception_incorrect_json_format})
    end.