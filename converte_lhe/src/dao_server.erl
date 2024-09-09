-module(dao_server).
-behaviour(gen_server).


-export([start_link/0]).
-export([registerUser/1, registerPassword/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


registerPassword(Email, Pass) -> gen_server:call(?MODULE, {register_pass, Email, Pass}).
registerUser(Email) -> gen_server:call(?MODULE, {register_user, Email}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Key = os:getenv("SUPABASE_KEY"),
    Url = os:getenv("SUPABASE_URL"),
    Headers = [
        {"apikey", Key},
        {"Authorization", "Bearer " ++ Key}
    ],
    State0 = [Url, Headers],
    {ok, State0}.

% url is the url of the table not the project
% in case of adding more tables it will be necessary to modify this
% Email is required to be a binary
handle_call({register_user, Email}, _From, [Url, Headers]) -> 
    httpc:request(
        post,
        {   
            Url, 
            Headers, 
            "application/json", 
            binary_to_list(jsx:encode(#{<<"email">> => Email}))
        }, 
        [], []),
    {reply, ok, [Url, Headers]};

handle_call(_, _From, State) -> {reply, false, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.