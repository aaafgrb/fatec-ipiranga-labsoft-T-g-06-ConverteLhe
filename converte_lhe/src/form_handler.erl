-module(form_handler).
-behavior(cowboy_handler).

-export([init/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!! unfinished !!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!! unfinished !!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!! unfinished !!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%todo: make the exceptions and their catches
init(Req0, State) ->
    R = try main_model:main(getInputData(Req0), getInputComposition(Req0))
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
    {ok, Req, State}.



%-----------------------------------------------------------------------

% gets the input data from the form file
%  !! ignoring inTxtBox and apiKey for now
%  expection only one file, the rest will be ignored
getInputData(Req0) -> 
    try
        {Form, _Req1} = acc_multipart(Req0),
        erlang:display(Form),

        erlang:display([{binary:match(element(2, maps:find(<<"content-disposition">>, M)), <<"name=\"formFile\"">>) , maps:find(<<"content-type">>, M), D} || {M, D} <- Form]),

        erlang:display([D || {_M, D} <- Form]),

        % if somehow the name of the file or some other part of the content-disposition 
        %   contains name=\"formFile\" it might produce false positives
        % !! todo: make this safer
        R = [D || {M, D} <- Form
            , binary:match(element(2, maps:find(<<"content-disposition">>, M)), <<"name=\"formFile\"">>) =/= nomatch
            , maps:find(<<"content-type">>, M) =:= {ok, <<"text/plain">>} 
        ],
        erlang:display(R),
        R
    of 
        [] -> throw(exception_no_data);
        [H|_] -> [erlang:binary_to_list(X) || X <- binary:split(H, [<<13>>, <<10>>], [global])];
        E -> erlang:display(E), throw(no_catch)
    catch
        _:E -> erlang:display(E), throw({exception_catch})
    end.

% gets the url parameter "comp"
getInputComposition(Req0) ->
    case cowboy_req:match_qs([{comp,[], <<>>}], Req0) of
        #{comp := <<>>} -> throw({exception_empty_composition});
        #{comp := V}    -> binary_to_list(V)
    end.

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