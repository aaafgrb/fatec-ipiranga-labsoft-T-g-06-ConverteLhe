-module(dao).

-export([register_user/1, register_password/2, get_user_data/2, get_user_remaining_limit/1, spend_usage/1]).

% todo: 
%   find a safer (and/or prettier) way to concat the email string
%   email validation


% register the email for a password change
register_user(Email) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Sender = ets:lookup_element(conf_table, smtp_sender, 2),

    % request the creation of the email's account
    % expects the password setting key as a return
    Content = "{\"e\": \"" ++ Email ++ "\"}",
    R = send_request(Url ++ "rpc/newUser", Headers, "application/json", Content),

    % removes the extra quotations
    F = case R of
        [34|V] -> lists:droplast(V);
        V -> V
    end,

    % sends the email
    Body = "Set Convertelhe password: " ++ ets:lookup_element(conf_table, dns_url, 2) ++ "confirmpass?k=" ++ F,
    gen_smtp_client:send(
        { Sender
        , [Email]
        , "Subject: Set Password\r\nFrom: ConverteLhe <" ++ Sender ++ 
            ">\r\nTo: user <" ++ Email ++ ">\r\n\r\n" ++ Body
        },
        [ {relay, ets:lookup_element(conf_table, smtp_relay, 2)}
        , {username, ets:lookup_element(conf_table, smtp_user, 2)}
        , {password, ets:lookup_element(conf_table, smtp_pass, 2)}
        , {port, ets:lookup_element(conf_table, smtp_port, 2)}
        ]),

    F.


% changes the password of a registered email
% the key is the key received by the registerUser request
register_password(Key, Pass) ->
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Content = "{\"p\": \"" ++ Pass ++ "\", \"k\": \"" ++ Key ++ "\"}",
    send_request(Url ++ "rpc/newPass", Headers, "application/json", Content).


% receives the apikey of the user
get_user_data(Email, Pass) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Content = "{\"p\": \"" ++ Pass ++ "\", \"e\": \"" ++ Email ++ "\"}",
    R = send_request(Url ++ "rpc/getKey", Headers, "application/json", Content),
    
    % removes the extra quotations
    case R of
        [34|V] -> lists:droplast(V);
        V -> V
    end.

% receives the limit - current usage of the user
get_user_remaining_limit(Key) ->
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Content = "{\"k\": \"" ++ Key ++ "\"}",

    try   R = send_request(Url ++ "rpc/getRemUsage", Headers, "application/json", Content), 
          list_to_integer(R)
    of    V -> V
    catch _:_ -> 0
    end.

% spend usage (only using 1 for now, idenpendent of the composition complexity, or if it will fail)
spend_usage(Key) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Content = "{\"k\": \"" ++ Key ++ "\"}",

    try   send_request(Url ++ "rpc/useRemUsage", Headers, "application/json", Content)
    of    "true" -> true
    catch _:_ -> false
    end.
    

%--------------------------------------------------------------------------------

send_request(Url, Headers, ContentType, Content) -> 
    try     
        httpc:request(post, {Url, Headers, ContentType, Content}, [], [])
    of
        {ok, {{_, 200, _}, _, R}} -> R;
        {ok, {{_, N, _}, _, _}} -> throw({exception_notok_request, N})
    catch
        _:_ -> throw({exception_failed_request})
    end.
