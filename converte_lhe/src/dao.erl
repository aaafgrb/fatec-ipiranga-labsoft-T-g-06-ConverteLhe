-module(dao).

-export([registerUser/1]).

%registerPassword(Email, Pass) -> gen_server:call(?MODULE, {register_pass, Email, Pass}).

% todo: find a safer (and/or prettier) way to concat the email string

% url is the url of the table not the project
% in case of adding more tables it will be necessary to modify this
% Email is required to be a list
registerUser(Email) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Sender = ets:lookup_element(conf_table, smtp_sender, 2),
    Body = "ababa123",

    httpc:request(
        post,
        {   
            Url, 
            Headers, 
            "application/json", 
            "{\"email\": \"" ++ Email ++ "\"}"
        }, 
        [], []),

    gen_smtp_client:send(
        { Sender
        , [Email]
        , "Subject: Confirm registration\r\nFrom: ConverteLhe <" ++ Sender ++ 
            ">\r\nTo: Some Dude <" ++ Email ++ ">\r\n\r\n" ++ Body
        },
        [ {relay, ets:lookup_element(conf_table, smtp_relay, 2)}
        , {username, ets:lookup_element(conf_table, smtp_user, 2)}
        , {password, ets:lookup_element(conf_table, smtp_pass, 2)}
        , {port, ets:lookup_element(conf_table, smtp_port, 2)}
        ]),

    ok.

% ignoring this for now
%validadeEmail(Email) -> true.

% validade email
% send email
% try catch
