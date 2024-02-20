## Nova



Nova is a web framework that is built on top of the Cowboy web server.



One of our goals is that it should be easy to start a web project in Erlang and maintain and operate it. With that, we have created a rebar3 plugin that not only template modules for you but also bootstrap applications, so you get something up and running fast.



Nova is a View-Controller right now, we are working on getting models and databases into it but have yet to arrive. As views, we use today erlydtl.



## Template a new Nova



`sh -c "$(curl -fsSL https://raw.githubusercontent.com/novaframework/rebar3_nova/master/install.sh)"`

When the rebar3 plugin is installed, you can template a new Nova app with the command.

`rebar3 new nova my_first_nova`

```
rebar3 new nova my_first_nova
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

If you are using `asdf` you can install the tools needed, versions of Erlang and rebar3 are in the `.tool-versions` file that comes with the templating.

### enotify

In the rebar3 nova plugin, we also use [enotify](https://github.com/tsloughter/enotify). It has some dependencies that need to be installed.

**Backends**
Mac fsevent
Linux inotify
Windows inotify-win
NOTE: On Linux, you need to install inotify-tools.

Subscribe to Notifications

```
> enotify:start_link("./").
> flush().
Shell got {".../enotify/src/README.md",[closed,modified]}}
```

When enotify is installed and working we can use the `rebar3 nova serve` command. That will work as `rebar3 shell` but it will look for files that have been changed and recompile them. It is very nice when you develop that you don't need to recompile your node each time you want to change something or develop something new.

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