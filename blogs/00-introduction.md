## Nova

### What is Nova?

[Nova](https://novaframework.org) is a web framework built on top of the [Cowboy](https://github.com/novaframework/nova) web server. It is open-source, and we are happy to receive contributions.

One of our goals is that it should be easy to start a web project in Erlang, and to maintain and operate it. With that, we have created [rebar3_nova](https://github.com/novaframework/rebar3_nova), which not only templates modules for you but also bootstraps applications, so you can get something up and running fast.

Nova is a View-Controller right now. We are working on integrating models and databases into it but have yet to do so. As for views, we currently use erlydtl.

### Why was Nova created?

Nova was created after some time working with Cowboy. We often found ourselves needing to quickly create new applications, and we didn't want to waste time creating new templates or copying code from the latest project into a new one. We wanted a way to bootstrap an application so we could start creating our application right away. We wanted a good way that we could shorten time to market when starting a new project.

One other thing was that when we worked with Cowboy REST, we often found ourselves adding the callback functions we needed in each handler. It could be the same code for authentication or the same code for other callbacks. Sometimes, we needed a way to share this code between different handlers for different routes.

Also, when it came to maintenance and operation, we often found that some handlers failed. We needed to understand the flow of all callbacks and then find which function was used to understand how the data was in the function. In fast-paced environments today, you don't want to spend time understanding the running code. Just find the module and function and solve the issue.


### Documentation

If you want to read the documentation about Nova you can find it [here](https://hexdocs.pm/nova/quick-start.html).