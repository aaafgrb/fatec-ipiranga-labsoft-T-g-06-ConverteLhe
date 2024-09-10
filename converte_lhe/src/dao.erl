-module(dao).

-export([registerUser/1]).

%registerPassword(Email, Pass) -> gen_server:call(?MODULE, {register_pass, Email, Pass}).

% url is the url of the table not the project
% in case of adding more tables it will be necessary to modify this
% Email is required to be a binary
registerUser(Email) -> 
    Url = ets:lookup_element(supabase_conf, url, 2),
    Headers = ets:lookup_element(supabase_conf, auth_headers, 2),
    httpc:request(
        post,
        {   
            Url, 
            Headers, 
            "application/json", 
            binary_to_list(jsx:encode(#{<<"email">> => Email}))
        }, 
        [], []),
    {reply, ok, [Url, Headers]}.
