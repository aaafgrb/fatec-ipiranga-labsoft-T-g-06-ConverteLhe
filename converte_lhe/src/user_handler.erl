-module(user_handler).
-behavior(cowboy_handler).

-export([init/2]).

% todo
%   make try catches
%   test all paths
%   make the password setting url handler

init(Req0, State) ->
    Content = try 
        D = getInputData(Req0),
        handleReq(D)
    of
        false -> #{<<"value">> => <<"">>,            <<"error">> => <<"">>};
        R     -> #{<<"value">> => list_to_binary(R), <<"error">> => <<"">>}
    catch
        {exception_unsupported_req, R}    -> #{<<"value">> => <<"">>, <<"error">> => <<"unsupported requisition", R/binary>>};
        {exception_incorrect_json_format} -> #{<<"value">> => <<"">>, <<"error">> => <<"incorrect json format">>};
        {exception_notok_request, N}      -> #{<<"value">> => <<"">>, 
                                               <<"error">> => list_to_binary("request failed: " ++ (integer_to_list(N)))};
        _:_                               -> #{<<"value">> => <<"">>, <<"error">> => <<"unexpected error">>}
    end,

    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Content), Req0),
    {ok, Req, State}.


% gets the input data from the request json
getInputData(Req0) -> 
    try
        {ok, JsonBin, _} = read_body(Req0, <<>>),
        Json = jsx:decode(JsonBin),
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

handleReq({"login"     , Email,_Key, Pass}) when (Email =/= false) and (Pass =/= false) -> dao:getUserData(Email, Pass);
handleReq({"changePass",_Email, Key, Pass}) when (Key =/= false)   and (Pass =/= false) -> dao:registerPassword(Key, Pass);
handleReq({"newPass"   , Email,_Key,_Pass}) when Email =/= false -> dao:registerUser(Email), "success";
handleReq({"newUser"   , Email,_Key,_Pass}) when Email =/= false -> dao:registerUser(Email), "success";

handleReq({Req, _Email, _Pass}) -> throw({exception_unsupported_req, list_to_binary(Req)}).

%--------------------------------------------------------------------------------

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.