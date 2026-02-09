---
title: "Create a New Nova Application"
weight: 2
---
## Create a new Nova application

We mentioned before that we wanted to get away from the hazzle of starting new things. To make it easier to install we have made this script. This one will check for rebar3 and if you don't have it install. Also it will add `rebar3_nova`, with that you will have some templates that will help you create your first Nova app.

```shell
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

When the rebar3 plugin is installed, you can template a new Nova app with the command.

Rebar3 `new` command that we will use can generate from start either and Erlang application or library.

```shell
$ rebar3 new app this_is_an_application
$ rebar3 new lib this_is_a_lib
```
But we want to create a Nova application, with `rebar3_nova` we will have a Nova template so we can write nova and the application name.

```shell
$ rebar3 new nova my_first_nova
```
This command will create a directory with the same name as your application, then it will add all the files needed to have a running Nova application.

```shell
===> Writing my_first_nova/config/dev_sys.config.src
===> Writing my_first_nova/config/prod_sys.config.src
===> Writing my_first_nova/src/my_first_nova.app.src
===> Writing my_first_nova/src/my_first_nova_app.erl
===> Writing my_first_nova/src/my_first_nova_sup.erl
===> Writing my_first_nova/src/my_first_nova_router.erl
===> Writing my_first_nova/src/controllers/my_first_nova_main_controller.erl
===> Writing my_first_nova/rebar.config
===> Writing my_first_nova/config/vm.args.src
===> Writing my_first_nova/priv/assets/favicon.ico
===> Writing my_first_nova/src/views/my_first_nova_main.dtl
===> Writing my_first_nova/.tool-versions
===> Writing my_first_nova/.gitignore
```

`.tool-versions` is used by asdf to know what versions of different packages that we can use here.
```shell
erlang 26.2.2
rebar 3.22.1
```

If we now use asdf it will install Erlang with version 26.2.2 and rebar3 version 3.22.1.

```shell
asdf install
```

asdf will now start downloading and installing both Erlang and Rebar3.

In `src/` is our source code, in this library you will have two other libraries. One is `src/controllers` here are the modules that we call controllers, they contain the logic of your application. `src/views` contains the django templates.

`rebar.config` is for telling rebar3 how to build our application and also if we want to add dependencies or how we do releases. I will dig into this deeper later.

In the `config/` directory you have two configuration files for your application, `dev_sys.config.src` and `prod_sys.config.src`. When we develop we use two commands in rebar3, first one is:
```shell
rebar3 shell
```

This will compile your code and start an Erlang shell, this is what we call development mode, so it will help you with dependencies and load all code. With the rebar3_nova plugin we have created a new command that is called:

```shell
rebar3 nova serve
```

This command will do the same as `rebar3 shell` but with the addition that it will get notification if a file was changed and compile it for you and re-load it. While you develop and save your file the node will compile and re-load so you don't need to restart everything.

Both of these commands when started will use the `dev_sys.config.src` configuration.

### Run our example

```shell
$ rebar3 nova serve
```
When the node is up you can start a browser and go to `localhost:8080` this will show a homepage telling you that the system is up. The other way to see if your system is up is to `curl`heartbeat endpoint.

```shell
curl -v localhost:8080/heartbeat
*   Trying 127.0.0.1:8080...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 8080 (#0)
> GET /heartbeat HTTP/1.1
> Host: localhost:8080
> User-Agent: curl/7.68.0
> Accept: */*
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< content-length: 0
< date: Fri, 26 Jul 2024 08:39:45 GMT
< server: Cowboy/Nova
< vary: accept-encoding
< set-cookie: session_id=60dl0UW8dnaUPU7DZ24GfF99+yTO2KAPzrWwUo0B0Zg=
< 
* Connection #0 to host localhost left intact
```



To list our routes, we can use `rebar3 nova routes`

```shell
Host: '_'
     ├─  /assets
        └─  _ /[...] (my_first_nova, cowboy_static:init/1)
     └─  GET / (my_first_nova, my_first_nova_main_controller:index/1)

```
