-module(converte_lhe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
	{'_', [
            {"/", cowboy_static, {priv_file, converte_lhe, "/pages/index.html"}},
            {"/index.js", cowboy_static, {priv_file, converte_lhe, "/pages/index.js"}},
            {"/api", json_handler, []},
            {"/form", form_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    converte_lhe_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http_listener).
