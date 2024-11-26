-module(user_model).
-export([handle_req/1]).

handle_req({"login"     , Email,_Key, Pass}) when (Email =/= false) and (Pass =/= false) -> dao:get_user_data(Email, Pass);
handle_req({"changePass",_Email, Key, Pass}) when (Key =/= false)   and (Pass =/= false) -> dao:register_password(Key, Pass);
handle_req({"newPass"   , Email,_Key,_Pass}) when Email =/= false -> dao:register_user(Email), "success";
handle_req({"newUser"   , Email,_Key,_Pass}) when Email =/= false -> dao:register_user(Email), "success";
handle_req({"getLimit"  ,_Email, Key,_Pass}) when Key =/= false   -> integer_to_list(dao:get_user_remaining_limit(Key));

handle_req({Req, _Email, _Key, _Pass}) -> throw({exception_unsupported_req, list_to_binary(Req)}).