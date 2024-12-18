-module(main_controller).

-export([main/3]).

% generic controller for the handlers that receive processing requests
main(Req0, State, UnpackFun) ->
    
    Content = 
        try
            Data = try UnpackFun(Req0) catch _:_ -> throw({exception_read_request}) end,
            main_model:main(Data)
        of
            {true, Res}  -> #{<<"result">> => Res, <<"error">> => <<"">>};
            {false, Err} -> #{<<"result">> => <<"">>, <<"error">> => Err};
            _            -> #{<<"result">> => <<"">>, <<"error">> => <<"unexpected error">>}
        catch
            {exception_read_request} -> #{<<"result">> => <<"">>, <<"error">> => <<"wasn't able to read the request, please check the documentation">>};
            _:_ -> #{<<"result">> => <<"">>, <<"error">> => <<"unexpected error">>}
        end,
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(Content), Req0),
    {ok, Req, State}.