-module(my_first_nova_auth).

-export([username_password/1]).

username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} -> {true, #{authed => true,
                                                         username => Username}};
        _ -> false
    end.
