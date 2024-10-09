-module(dao).

-export([register_user/1, register_password/2, get_user_data/2]).

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
    Body = "confirm Convertelhe registration: " ++ ets:lookup_element(conf_table, dns_url, 2) ++ "confirmpass?k=" ++ F,
    gen_smtp_client:send(
        { Sender
        , [Email]
        , "Subject: Confirm registration\r\nFrom: ConverteLhe <" ++ Sender ++ 
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
