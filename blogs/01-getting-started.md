
## Nova

### What is Nova?

[Nova](https://novaframework.org) is a web framework built on top of the [Cowboy](https://github.com/novaframework/nova) web server. It is open-source, and we are happy to receive contributions.

One of our goals is that it should be easy to start a web project in Erlang, and to maintain and operate it. With that, we have created [rebar3_nova](https://github.com/novaframework/rebar3_nova), which not only templates modules for you but also bootstraps applications, so you can get something up and running fast.

Nova is a View-Controller right now. We are working on integrating models and databases into it but have yet to do so. As for views, we currently use erlydtl.

### Why was Nova created?

Nova was created after some time working with Cowboy. We often found ourselves needing to quickly create new applications, and we didn't want to waste time creating new templates or copying code from the latest project into a new one. We wanted a way to bootstrap an application so we could start creating our application right away. We wanted a good way that we could shorten time to market when starting a new project.

One other thing was that when we worked with Cowboy REST, we often found ourselves adding the callback functions we needed in each handler. It could be the same code for authentication or the same code for other callbacks. Sometimes, we needed a way to share this code between different handlers for different routes.

Also, when it came to maintenance and operation, we often found that some handlers failed. We needed to understand the flow of all callbacks and then find which function was used to understand how the data was in the function. In fast-paced environments today, you don't want to spend time understanding the running code. Just find the module and function and solve the issue.

## Create a new Nova application

```shell
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"
```

When the rebar3 plugin is installed, you can template a new Nova app with the command.

```shell
$ rebar3 new nova my_first_nova
```


```shell
$ rebar3 new nova my_first_nova
===> Skipping thoas v1.2.0 as an app of the same name has already been fetched
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

### asdf

asdf is a version manager system that helps you use the correct tools when developing an application. This is not only for Erlang, it can be used for many areas you can read more about asdf [here](https://asdf-vm.com/).

When we bootstrap our application we will get a `.tool-versions` file that includes Erlang and Rebar3 versions that you can install with asdf.

### enotify

In the rebar3 nova plugin, we also use [enotify](https://github.com/tsloughter/enotify).

Depending on your system you will need to install the dependency that is listed below.

**Backends**
Mac fsevent
Linux inotify
Windows inotify-win
NOTE: On Linux, you need to install inotify-tools.

When enotify is installed and working, we can use the `rebar3 nova serve` command. That will work as `rebar3 shell` but it will look for files that have been changed and recompile them. It is very nice when you develop, that you don't need to recompile your node each time you want to change something or develop something new.

### Run our example

```
cd my_first_nova/
rebar3 nova serve
```
When the node is up you can start a browser and go to `localhost:8080` this will show a homepage telling you that the system is up.

To list our routes, we can use `rebar3 nova routes`

```
Host: '_'
     ├─  /assets
        └─  _ /[...] (my_first_nova, cowboy_static:init/1)
     └─  GET / (my_first_nova, my_first_nova_main_controller:index/1)

```