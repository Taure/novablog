-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", { my_first_nova_main_controller, index}, #{methods => [get]}},
                 {"/assets/[...]", "assets"}
                ]
      }].
