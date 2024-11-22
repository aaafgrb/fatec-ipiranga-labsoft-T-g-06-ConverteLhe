-module(form_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    main_controller:main(Req0, State, fun unpack_fun/1).

% extracts the Comp and Data
unpack_fun(Req0) -> 
    Pattern = "\\r\\n|\\r|\\n",
    {ok, LineSplitterRegex} = re:compile(Pattern),
    FormData = req_util:get_form_data(Req0),
    {_, Data} = lists:keyfind("formFile", 1, FormData),
    {_, Comp} = lists:keyfind("comp", 1, FormData),
    {_, ApiKey} = lists:keyfind("apiKey", 1, FormData),
    {_, OneString} = lists:keyfind("oneString", 1, FormData),
    Lines = case OneString of
        <<"true">> -> [Data];
        _          -> re:split(Data, LineSplitterRegex, [{return, list}])
    end,
    {[[X]||X <- Lines], binary_to_list(Comp), binary_to_list(ApiKey)}.
