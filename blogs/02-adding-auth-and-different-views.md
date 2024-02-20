In the previous section, we created a Nova application.

Now we will add a small login form and try to auth and if we pass it will show a view with the message "Welcome Daniel!".

The structure we have in applications using Nova is that in src/ we usually have modules that are used by our application. In the directory src/controllers, we will have Erlang modules that will be used to handle requests.
In the directory src/views, we have the .dtl files for our endpoints. The names need to match so MY_VIEW.dtl should match MY_VIEW_controller.erl.

Security is handled in our routing file. It looks like this.

```
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

```
Nova can have different routes depending on what you want to achieve. So for now we want to add a setting for endpoints that will use a security module.

```
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
      },
      #{prefix => "",
      security => {my_first_nova_auth, auth},
      plugins => [{pre_request, nova_request_plugin, #{read_urlencoded_body => true}}],
      routes => [
           {"/login", { my_first_nova_login_controller, index}, #{methods => [post]}}
               ]
    }].
```

When we add the input form it will submit a username and password on the endpoint `/login`. If it passes our auth module `my_first_nova_auth` it will show `my_first_nova_login.dtl` and also `my_first_nova_login_controller` will have the logic. One other thing is that we use a plugin here called `nova_request_plugin` it takes a map as a configuration and we say that we want it to read the urlencoded body. We will get a key called `params` in the Req that will contain a erlang map.

First, we change the view for our main page my_first_nova_main.dtl.

```
<html>
<body>
  <div>
    <form action="/login" method="post" id="nameform">
      <label for="username">userame:</label>
      <input type="text" id="username" name="username"><br>
      <label for="password">Password:</label>
      <input type="password" id="password" name="password"><br>
      <input type="submit" value="submit">
    </form>
  </div>
</body>
</html>
```
We have added an input form now that has a username and password. When we click on the submit button it will trigger the auth module.

my_first_nova_auth.erl, we specified in our routing file what the module should be called and the function that will be used.

```
-module(my_first_nova_auth).

-export([auth/1]).

auth(#{params := Params}) ->
    case Params of
	#{<<"username">> := <<"daniel">>,
      <<"password">> := <<"test">>} ->
	    {true, #{<<"username">> => <<"daniel">>,
		         <<"authed">> => true}};
	_ ->
	    {true, #{<<"authed">> => false}}
    end.
```

Input will send data as a string so that is what the plugin will parse for us.

Data here will be a binary string `<<"username=USERNAME&password=PASSWORD">>`.

When we return a tuple with {true, map()}, the map will be added to the Req-map as a field called `auth_data`. If we had a rest API or want to send back a 401 to the one that did the request we return false in our auth module. But in this case, we want to redirect back to `/` if you pass in the wrong credentials. If we did enter the correct password and username we are going to show `Welcome USERNAME!`.

We need to create the controller now and the view for this, `my_first_nova_login_controller.erl` in `src/controllers/`.

```
-module(my_first_nova_login_controller).

-export([index/1]).

index(#{auth_data := #{<<"authed">> := true,
	                   <<"username">> := Username}}) ->
    {ok, [{username, Username}]};
index(#{auth_data := #{<<"authed">> := false}}) ->
    {redirect, "/"}.

```
In the controller, we have an index function that takes one argument, which is a cowboy request.

And then the view `my_first_nova_login.dtl` should be created in `src/views/`
```
<html>
<body>
<h1> Welcome {{ username }}!</h1>
</body>
</html>
```

What will happen here is that if we have the correct username and password the auth module will pass on the map we are giving in it. If authentication are false it will redirect back to `/` if it returns true we should print the Username.

In this case, we didn't use any database or so to have users, just to show how things work. If you want to see this with what you enter in the username we can change the auth module to.

```
-module(my_first_nova_auth).

-export([auth/1]).

auth(#{params := Params}) ->
    case Params of
	#{<<"username">> := Username,
      <<"password">> := <<"test">>} ->
	    {true, #{<<"username">> => Username,
		         <<"authed">> => true}};
	_ ->
	    {true, #{<<"authed">> => false}}
    end.
```

In this case, Username will be passed on to the controller that will print it on the page.