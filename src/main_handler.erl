-module(main_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        cowboy_req:binding(comp, Req0),
        Req0),
    {ok, JsonBin, _} = read_body(Req0, <<>>),
    erlang:display(jsx:decode(JsonBin)),
    {ok, Req, State}.


read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
