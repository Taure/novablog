---
title: "Introduction to Nova and Prerequisites"
date: 2024-07-01T00:00:00+00:00
weight: 1
tags: ["nova", "erlang", "getting-started"]
series: ["Nova Framework Guide"]
summary: "What is the Nova Framework, why use it, and what you need to get started."
---


## Nova

### What is Nova?

One of our goals is that it should be easy to start a web project in Erlang and to maintain and operate it. With that, we have created [rebar3_nova](https://github.com/novaframework/rebar3_nova), which not only templates modules for you but also bootstraps applications, so you can get something up and running fast.

Nova is a View-Controller right now. We are working on integrating models and databases into it but have yet to do so. As for views, we currently use erlydtl.

### Why should I use Nova?

Nova is the solution for rapid application development. Born from the frustrations of repetitive setup tasks in Cowboy, it offers quick application bootstrapping, eliminates code redundancy, and provides clear visibility into your application's flow. With Nova, you'll slash time-to-market, enhance code reusability, and troubleshoot with ease. Streamline your workflow and conquer modern development challenges with Nova.


## Pre-requirements for this series

Nova is using Erlang as a language and Rebar3 as a tool to compile. I will use asdf in this tutorial for how to install Erlang and Rebar3, if you want to install them using other ways you can read at [Adopting Erlang](https://adoptingerlang.org/docs/development/setup/) on how to install them.

### Rebar3

Rebar3 is the tool we will use for building our application and creating releases and even documentation. Rebar3 has something called plugins, which is code that helps us with some functionality. We will next article introduce you to `rebar3_nova` a Rebar3 plugin that makes developing and working with Nova much smoother.

### asdf

asdf is a version manager system that helps you use the correct tools when developing an application. This is not only for Erlang, it can be used for many areas you can read more about asdf [here](https://asdf-vm.com/).

When we bootstrap our application we will get a `.tool-versions` file that includes Erlang and Rebar3 versions that you can install with asdf.

How to install `asdf`, you can read [here](https://asdf-vm.com/guide/getting-started.html) for more details.

```shell
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0

```

This will install the asdf system, and then you will need to add it to your shell, I will show bash but if you are using other shell you can read [here](https://asdf-vm.com/guide/getting-started.html#_3-install-asdf).

**Bash & Git**
Add the following to `~/.bashrc`:
```shell
. "$HOME/.asdf/asdf.sh"
```
Completions must be confgured by adding the following to your `.bashrc`:
```shell
. "$HOME/.asdf/completions/asdf.bash"
```

After that asdf is installed we need to add the Rebar plugin to asdf.

```shell
asdf plugin-add rebar https://github.com/Stratus3D/asdf-rebar.git
```

And also the Erlang plugin.

```shell
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
```

We will need these plugins later when we start Nova.

### enotify

In the rebar3 nova plugin, we also use [enotify](https://github.com/tsloughter/enotify).

Depending on your system you will need to install the dependency that is listed below.

**Backends**
Mac fsevent
Linux inotify
Windows inotify-win
NOTE: On Linux, you need to install inotify-tools.

When enotify is installed and working, we can use the `rebar3 nova serve` command. That will work as `rebar3 shell` but it will look for files that have been changed and recompile them. It is very nice when you develop, that you don't need to recompile your node each time you want to change something or develop something new.


## In this serie

In this serie I will write about how you create a Nova application with a web login that also can include other Nova applications.