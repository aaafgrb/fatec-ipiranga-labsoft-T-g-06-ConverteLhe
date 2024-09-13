-module(dao).

-export([registerUser/1, registerPassword/2, getUserData/2]).

% todo: 
%   find a safer (and/or prettier) way to concat the email string
%   email validation


% register the email for a password change
registerUser(Email) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Sender = ets:lookup_element(conf_table, smtp_sender, 2),

    {ok, {{_, 200, _}, _, K}} = httpc:request(
        post,
        {   
            Url ++ "rpc/newUser", 
            Headers, 
            "application/json", 
            "{\"e\": \"" ++ Email ++ "\"}"
        }, 
        [], []),
    
    % removes the extra quotations
    F = case K of
        [34|V] -> lists:droplast(V);
        V -> V
    end,

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
registerPassword(Key, Pass) ->
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Enc = encrypt(Pass),
    case     
        httpc:request(
            post,
            {   
                Url ++ "rpc/newPass", 
                Headers, 
                "application/json", 
                "{\"p\": \"" ++ Enc ++ "\", \"k\": \"" ++ Key ++ "\"}"
            }, 
            [], [])
    of
        {ok, {{_, 200, _}, _, R}} -> R;
        {ok, {{_, N, _}, _, _}} -> throw({exception_failed_request, N})
    end.


% received the apikey of the user
getUserData(Email, Pass) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Enc = encrypt(Pass),
    {ok, {{_, 200, _}, _, R}} =  httpc:request(
        post,
        {   
            Url ++ "rpc/getKey", 
            Headers,
            "application/json", 
            "{\"p\": \"" ++ Enc ++ "\", \"e\": \"" ++ Email ++ "\"}"
        }, 
        [], []),
    % removes the extra quotations
    case R of
        [34|V] -> lists:droplast(V);
        V -> V
    end.
%--------------------------------------------------------------------------------

encrypt(Data) -> 
    Salt = ets:lookup_element(conf_table, hash_salt, 2),
    Hash = crypto:hash(sha512, Data ++ Salt),
    <<SHA512:512/big-unsigned-integer>> = Hash,
    io_lib:format("~111.36.0b", [SHA512]).
