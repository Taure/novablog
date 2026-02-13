-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{routes => [
        {404, fun my_first_nova_error_controller:not_found/1, #{}},
        {500, fun my_first_nova_error_controller:server_error/1, #{}}
     ]},
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/ws", my_first_nova_ws_handler, #{protocol => ws}}
                ]
      },
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [{"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}]
     },
    #{prefix => "/api",
      security => false,
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
                 {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
                 {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}},
                 {"/products", fun my_first_nova_products_controller:list/1, #{methods => [get]}},
                 {"/products/:id", fun my_first_nova_products_controller:show/1, #{methods => [get]}},
                 {"/products", fun my_first_nova_products_controller:create/1, #{methods => [post]}},
                 {"/products/:id", fun my_first_nova_products_controller:update/1, #{methods => [put]}},
                 {"/products/:id", fun my_first_nova_products_controller:delete/1, #{methods => [delete]}}
                ]
     },
    #{prefix => "/api",
      security => false,
      routes => [
                 {"/metrics", fun my_first_nova_metrics_controller:summary/1, #{methods => [get]}},
                 {"/metrics/recent", fun my_first_nova_metrics_controller:recent/1, #{methods => [get]}}
                ]
     }
   ].
