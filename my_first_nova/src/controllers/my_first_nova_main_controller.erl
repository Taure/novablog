-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1
        ]).

index(#{auth_data := #{authed := true,
                       username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {status, 401}.

login(_Req) ->
    {ok, [], #{view => login}}.