-module(json_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    main_controller:main(Req0, State, fun unpack_fun/1).

% extracts the Comp and Data
unpack_fun(Req0) -> 
    {ok, Json, _Req} = req_util:read_body(Req0, <<>>),
    {ok, Value} = maps:find(<<"processThis">>, Json),
    {ok, ApiKey} = maps:find(<<"apiKey">>, Json),    
    Data = [[X] || X <- Value],
    Comp = req_util:get_url_param(Req0, comp),
    {Data, Comp, binary_to_list(ApiKey)}.