## Adding authd and a new view

In earlier sections, we have talked about routings and plugins.

Now we will add a small login form. We'll atempt to authenticate, and if succesful, it will display a view with the message "Welcome Daniel!".

In applications using Nova, our typical structure involves having modules in the `src/` directory that are utilized by our application. Within the `src/controllers` directory, we'll have Erlang modules dedicated to handling requests.

In the directory `src/views`, we have the `.dtl` files for our endpoints. It's important to ensure consistency in naming; therefore, `MY_VIEW.dtl` should correspond with `MY_VIEW_controller.erl`.

Security is managed within our routing file, which is structured as follows.

```erlang
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
Nova offers flexibility with its routing options, allowing customization based on specific objectives. Currently, our focus is on configuring endpoints to utilize a security module.

```erlang
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

Upon incorporating the input form, users will submit a username and password to the `/login` endpoint. Upon successful authentication by our `my_first_nova_auth` module, the system will display `my_first_nova_login.dtl`, and the logic will be handled by `my_first_nova_login_controller`. One other thing is that we use a plugin here called `nova_request_plugin` it takes a map as a configuration. Additionally, we utilize a plugin called `nova_request_plugin`, which is configured to read the urlencoded body. We will receive a key called `params` in the Req, which will contain an Erlang map.

First, we change the view for our main page my_first_nova_main.dtl.

```html
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
We have added an input form now that has a username and password. Clicking the submit button triggers the authentication module.

`my_first_nova_auth.erl`, we specified in our routing file what the module should be called and the function that will be used.

```erlang
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

Once data is submitted, the authentication module processes it as a erlang map, which is patterned matched by the plugin.

When we return a tuple with `{true, map()}`, the map will be added to the Req-map as a field called `auth_data`. If we had a rest API or want to send back a 401 to the one that did the request we return false in our auth module. If authentication fails, the system redirects back to `/`. However, if authentication succeeds, the system displays the `Welcome USERNAME!` message.

We need to create the controller now and the view for this, `my_first_nova_login_controller.erl` in `src/controllers/`.

```erlang
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
```html
<html>
<body>
<h1> Welcome {{ username }}!</h1>
</body>
</html>
```

What will happen here is that if we have the correct username and password the auth module will pass on the map we are giving in it. If authentication are false it will redirect back to `/` if it returns true we should print the Username.

In this case, we didn't use any database or so to have users, solely to demonstrate functionality.. If you want to see this with what you enter in the username we can change the auth module to.

```erlang
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