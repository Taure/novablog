-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(#{}) ->
    {ok, [], #{view => login}}.