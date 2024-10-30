-module(user_handler_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_login1_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"login">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, get_user_data, fun(_Email,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"\",\"value\":\"passkey12345\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_login2_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"login">>, <<"email">> => <<"e">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, get_user_data, fun(_Email,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"unsupported requisition: login\",\"value\":\"\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_changePass1_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"changePass">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_password, fun(_Key,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"\",\"value\":\"passkey12345\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_changePass2_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"changePass">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_password, fun(_Key,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"unsupported requisition: changePass\",\"value\":\"\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).


init_newPass1_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"newPass">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_user, fun(_Email) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"\",\"value\":\"success\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_newPass2_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"newPass">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_user, fun(_Email) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"unsupported requisition: newPass\",\"value\":\"\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).


init_newUser1_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"newUser">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_user, fun(_Email) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"\",\"value\":\"success\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_newUser2_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"newUser">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, register_user, fun(_Email) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"unsupported requisition: newUser\",\"value\":\"\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_noise_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"login">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>
            , <<"k1ey">> => <<"k">>, <<"k2ey">> => <<"k">>, <<"k3ey">> => <<"k">>, <<"k4ey">> => <<"k">>,<<"k5ey">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, get_user_data, fun(_Email,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"\",\"value\":\"passkey12345\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

init_inexistent_test() ->
    meck:new(cowboy_req, []),
    meck:expect(cowboy_req, reply, fun(Code, Type, Content, Req0) -> {Code, Type, Content, Req0} end ),

    meck:new(req_util, []),
    meck:expect(req_util, read_body, fun(_Req0, State) -> 
        {ok, #{<<"req">> => <<"lfogin">>, <<"email">> => <<"e">>, <<"pass">> => <<"p">>, <<"key">> => <<"k">>}, State} end ),

    meck:new(dao, []),
    meck:expect(dao, get_user_data, fun(_Email,  _pass) -> "passkey12345" end),
    
    R = user_handler:init([], []),
    % erlang:display(R),
    ?assertEqual(
        { ok
        , { 200
          , #{<<"content-type">> => <<"application/json">>}
          , <<"{\"error\":\"unsupported requisition: lfogin\",\"value\":\"\"}">>
          , []
          }
        , []
        }, 
        R), 

    ?assert(meck:validate(dao)),
    meck:unload(dao),
    ?assert(meck:validate(req_util)),
    meck:unload(req_util),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).



-endif.

