-module(dao).

-export([registerUser/1, registerPassword/2, getUserData/2]).

% todo: 
%   find a safer (and/or prettier) way to concat the email string
%   unpack the http response data
%   make the password setting url
%   email validation
%   try catch


% register the email for a password change
registerUser(Email) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Sender = ets:lookup_element(conf_table, smtp_sender, 2),

    K = httpc:request(
        post,
        {   
            Url ++ "rpc/newUser", 
            Headers, 
            "application/json", 
            "{\"e\": \"" ++ Email ++ "\"}"
        }, 
        [], []),
    erlang:display(K),

    
    Body = "ababa123 ",


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


% changes the password of a registered email
% the key is the key received by the registerUser request
registerPassword(Key, Pass) ->
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Enc = encrypt(Pass),
    R = httpc:request(
        post,
        {   
            Url ++ "rpc/newPass", 
            Headers, 
            "application/json", 
            "{\"p\": \"" ++ Enc ++ "\", \"k\": \"" ++ Key ++ "\"}"
        }, 
        [], []),
    erlang:display(R),
    ok.


% received the apikey of the user
getUserData(Email, Pass) -> 
    Url = ets:lookup_element(conf_table, db_url, 2),
    Headers = ets:lookup_element(conf_table, db_auth_headers, 2),
    Enc = encrypt(Pass),
    R = httpc:request(
        post,
        {   
            Url ++ "rpc/getKey", 
            Headers,
            "application/json", 
            "{\"p\": \"" ++ Enc ++ "\", \"e\": \"" ++ Email ++ "\"}"
        }, 
        [], []),
    erlang:display(R),
    ok.

%--------------------------------------------------------------------------------

encrypt(Data) -> 
    Salt = ets:lookup_element(conf_table, hash_salt, 2),
    Hash = crypto:hash(sha512, Data ++ Salt),
    <<SHA512:512/big-unsigned-integer>> = Hash,
    io_lib:format("~111.36.0b", [SHA512]).
