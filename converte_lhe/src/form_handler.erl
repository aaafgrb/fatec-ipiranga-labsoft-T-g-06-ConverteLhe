-module(form_handler).
-behavior(cowboy_handler).

-export([init/2]).

%todo: make the exceptions and their catches
init(Req0, State) ->
    Pattern = "\\r\\n|\\r|\\n",
    {ok, LineSplitterRegex} = re:compile(Pattern),
    R = try
            {Form, _Req1} = acc_multipart(Req0),
            FormData = getData(Form),
            {_, Data} = lists:keyfind("formFile", 1, FormData),
            {_, Comp} = lists:keyfind("comp", 1, FormData),
            Lines = re:split(binary_to_list(Data), LineSplitterRegex, [{return, list}]),
            main_model:main(Lines, binary_to_list(Comp))
        of V -> V
        catch
            _:E -> erlang:display(E), {false, <<"unexpected error">>}
        end,

    C = case R of
            {true, Res}  -> #{<<"result">> => Res, <<"error">> => <<"">>};
            {false, Err} -> #{<<"result">> => <<"">>, <<"error">> => Err};
            R            -> erlang:display(R), #{<<"result">> => <<"">>, <<"error">> => <<"unexpected error">>}
        end,
        Req = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jsx:encode(C), Req0),
    erlang:display(R),
    {ok, Req, State}.

%-----------------------------------------------------------------------

% tries to extract and format the data of the form
% returns a tuple with the 'name' field and the data binary
getData(Form) ->

    Regex = "name=\"([^\"]+)\"",
    {ok, RegexCompiled} = re:compile(Regex),

    lists:map(
        fun({M, D}) ->
            case re:run(M, RegexCompiled) of
                {match, [_,{S,E}|_]} -> {string:substr(M, S+1, E), D}
            end
        end,
        [{binary_to_list(element(2, maps:find(<<"content-disposition">>, M))), D} || {M, D} <- Form]).

%-----------------------------------------------------------------------

acc_multipart(Req0) -> acc_multipart(Req0, []).

acc_multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req} = stream_body(Req1, <<>>),
            acc_multipart(Req, [{Headers, Body}|Acc]);
        {done, Req} ->
            {lists:reverse(Acc), Req}
    end.

stream_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} ->
            stream_body(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} ->
            {ok, << Acc/binary, Data/binary >>, Req}
    end.