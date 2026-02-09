---
title: Adding a Nova App
weight: 7
---

## Adding a Nova app to your application

We have talked about views, we have touched controllers and we have talked about routing.

Now we will introduce how we can use someone else nova application inside yours. We will use [Nova Admin](https://github.com/novaframework/nova_admin) as an example. This is a nova application that lets you do observer kind of functionality.

### Adding the dependency

In our nova application we open `rebar.config` and add Nova admin as a dependency.

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"}, %% Used for logging - Change if needed
        {nova_admin, ".*", {git, "git@github.com:novaframework/nova_admin.git", {branch, "master"}}}
       ]}.
```

### Configuring the sub-application

Now we need to tell Nova about this new application. We do this in our `dev_sys.config.src` file. Nova has a configuration called `nova_apps` that we set in our own application's environment. This tells Nova to compile and mount routes from other Nova applications.

```erlang
{my_first_nova, [
    {nova_apps, [
        {nova_admin, #{prefix => "/admin"}}
    ]}
]}
```

The `prefix` option means all routes from `nova_admin` will be mounted under `/admin`. So if nova_admin has a route for `/` it will be available at `/admin/` in our application.

Your full `dev_sys.config.src` should now look something like this:

```erlang
[
  {kernel, [
    {logger_level, debug},
    {logger, [
      {handler, default, logger_std_h,
        #{formatter => {flatlog, #{
            map_depth => 3,
            term_depth => 50,
            colored => true,
            template => [colored_start, "[\033[1m", level, "\033[0m", colored_start, "] ", msg, "\n", colored_end]
          }}}}
    ]}
  ]},
  {nova, [
         {use_stacktrace, true},
         {environment, dev},
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {dev_mode, true},
         {bootstrap_application, my_first_nova},
         {plugins, [
                    {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
                   ]}
        ]},
  {my_first_nova, [
      {nova_apps, [
          {nova_admin, #{prefix => "/admin"}}
      ]}
  ]}
].
```

### How it works

When Nova starts up it reads the `bootstrap_application` setting and looks for the `nova_apps` configuration in that application's environment. It then compiles routes from all listed sub-applications and merges them into the routing tree.

Each sub-application is a standalone Nova app with its own router module. Nova Admin has a `nova_admin_router.erl` that defines its own routes. When we set `prefix => "/admin"`, Nova prepends that prefix to all of nova_admin's routes.

### Starting it up

Now we can fetch dependencies and start the node:

```shell
$ rebar3 nova serve
```

If we check our routes we should see nova_admin's routes mounted under `/admin`:

```shell
$ rebar3 nova routes
```

You can now go to `localhost:8080/admin` in your browser and you should see the Nova Admin interface. It gives you observer-like functionality directly in the browser, you can see running processes, memory usage and other information about your running Erlang node.

### Using other Nova apps

The same pattern works for any Nova application. If you find or build a Nova app that you want to include, you just:

1. Add it as a dependency in `rebar.config`
2. Add it to `nova_apps` in your sys.config with an optional prefix
3. Start your application

You can mount multiple sub-applications, each with their own prefix:

```erlang
{my_first_nova, [
    {nova_apps, [
        {nova_admin, #{prefix => "/admin"}},
        {my_api_app, #{prefix => "/api"}}
    ]}
]}
```

This is one of the powerful features of Nova. You can compose applications from multiple Nova apps, each handling their own piece of the system. In the next article we will look at how to build a JSON API with Nova.
