-module(json_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    main_controller:main(Req0, State, fun unpackFun/1).

% extracts the Comp and Data
unpackFun(Req0) -> 
    {ok, Json, _Req} = req_util:read_body(Req0, <<>>),
    {ok, Value} = maps:find(<<"processThis">>, Json),
    Data = [erlang:binary_to_list(Value)],
    Comp = req_util:get_url_param(Req0, comp),
    {Data, Comp}.