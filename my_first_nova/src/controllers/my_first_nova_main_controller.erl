-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1
        ]).

index(#{auth_data := #{username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]}.

login(_Req) ->
    {ok, [], #{view => login}}.