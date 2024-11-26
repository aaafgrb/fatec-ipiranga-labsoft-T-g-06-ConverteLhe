-module(req_util).

-export([get_form_data/1, get_url_param/2, read_body/2]).

% returns a list of tuples of {Name, Data}, 
%   where Name is the value of the name attribute of the content disposition
%   and Data is the binary Data
get_form_data(Req0) ->
    {Form, _Req1} = acc_multipart(Req0),

    Regex = "name=\"([^\"]+)\"",
    {ok, RegexCompiled} = re:compile(Regex),

    F = fun ({M, D}) ->
        ContentDisposition = binary_to_list(element(2, maps:find(<<"content-disposition">>, M))),
        case re:run(ContentDisposition, RegexCompiled) of
            {match, [_, {S, E} | _]} -> {string:substr(ContentDisposition, S + 1, E), D}
        end
    end,
    lists:map(F, Form).

% gets the url parameter 
get_url_param(Req0, Param) ->
    case cowboy_req:match_qs([{Param,[], <<>>}], Req0) of
        #{Param := <<>>} -> throw({exception_missing_url_parameter});
        #{Param := V}    -> binary_to_list(V)
    end.


% read json------------------------------------------------
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, jsx:decode(<< Acc/binary, Data/binary >>), Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

% read multipart-------------------------------------------
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