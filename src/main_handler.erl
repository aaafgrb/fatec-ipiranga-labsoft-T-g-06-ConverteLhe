-module(main_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Data = getInputData(Req0),
    Comp = getInputComposition(Req0),
    % make the arrow class accept binary instead of this in the future
    Res = main(
        lists:map(fun erlang:binary_to_list/1, Data), 
        erlang:binary_to_list(Comp)),

    % cowboy_req:binding(comp, Req0),
    erlang:display(Res),
    
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"asd">>, Req0),
    {ok, Req, State}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%-------------------------------------------------------------------------------

% gets the input data from the request json
getInputData(Req0) -> 
    {ok, JsonBin, _} = read_body(Req0, <<>>),
    Json = jsx:decode(JsonBin),
    % change this later
    case maps:find(<<"processThis">>, Json) of 
        error       -> [];
        {ok, Value} -> [Value]
    end.

getInputComposition(Req0) ->
    case cowboy_req:match_qs([{param,[], <<>>}], Req0) of
        #{param := V} -> V;
        <<>> -> <<>>
    end.

% handles solving processes
main([], _)   -> {false, <<"no request body">>};
main(_, <<>>) -> {false, <<"empty composition">>};
main(Data, Comp) -> 
    try 
        Composition = arrow:parseComposition(Comp),
        CompositionFun = arrow:resolveComposition(Composition),
        % think of the possibility of a worker pool here
        %   even if it fails make it continue???

        % is it possible that the Data isnt a string?
        lists:map(CompositionFun, Data)
    of
        Res -> { true, Res }
    catch
        { E, _V } -> erlang:display(E)
    end.
    

% sabc/x1/$concat/:2
% make it throw a propper error