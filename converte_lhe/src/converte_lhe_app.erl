-module(converte_lhe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    start_ets_table(),
    Dispatch = cowboy_router:compile([
	{'_', [
            {"/", cowboy_static, {priv_file, converte_lhe, "/pages/index.html"}},
            {"/index.js", cowboy_static, {priv_file, converte_lhe, "/pages/index.js"}},
            {"/connection.js", cowboy_static, {priv_file, converte_lhe, "/pages/connection.js"}},
            {"/connection.css", cowboy_static, {priv_file, converte_lhe, "/pages/connection.css"}},
            {"/grid.css", cowboy_static, {priv_file, converte_lhe, "/pages/grid.css"}},
            {"/user", cowboy_static, {priv_file, converte_lhe, "/pages/user.html"}},
            {"/user.js", cowboy_static, {priv_file, converte_lhe, "/pages/user.js"}},
            {"/confirmpass", cowboy_static, {priv_file, converte_lhe, "/pages/changePass.html"}},
            {"/changePass.js", cowboy_static, {priv_file, converte_lhe, "/pages/changePass.js"}},
            {"/Util.js", cowboy_static, {priv_file, converte_lhe, "/pages/Util.js"}},
            {"/functionList.js", cowboy_static, {priv_file, converte_lhe, "/pages/functionList.js"}},
            {"/components/[...]", cowboy_static, {priv_dir, converte_lhe, "/pages/components"}},
            {"/api", json_handler, []},
            {"/form", form_handler, []},
            {"/auseronn", user_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 80}],
        #{env => #{dispatch => Dispatch}}
    ),
    converte_lhe_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http_listener).

start_ets_table() -> 
    ets:new(
        conf_table, 
        [ named_table
        , set
        , protected
        , {keypos, 1}
        , {heir, none}
        , {read_concurrency, true}
        , {write_concurrency, false}
        , {decentralized_counters, false}
        ]),
    Key = os:getenv("CONVERTELHE_DB_KEY"),
    ets:insert(conf_table, {db_url, os:getenv("CONVERTELHE_DB_URL")}),
    ets:insert(conf_table, {db_auth_headers, [{"apikey", Key}, {"Authorization", "Bearer " ++ Key}]}),
    ets:insert(conf_table, {smtp_sender, os:getenv("CONVERTELHE_SMTP_SENDER")}),
    ets:insert(conf_table, {smtp_relay, os:getenv("CONVERTELHE_SMTP_RELAY")}),
    ets:insert(conf_table, {smtp_user, os:getenv("CONVERTELHE_SMTP_USER")}),
    ets:insert(conf_table, {smtp_pass, os:getenv("CONVERTELHE_SMTP_PASS")}),
    ets:insert(conf_table, {smtp_port, list_to_integer(os:getenv("CONVERTELHE_SMTP_PORT"))}),
    ets:insert(conf_table, {dns_url, os:getenv("CONVERTELHE_DNS_URL")}).


