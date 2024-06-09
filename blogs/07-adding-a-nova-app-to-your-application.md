## Adding a Nova app to your application

We have talked about views, we have touched controllers and we have talked about routing.

Now we will introduce how we can use someone else nova application inside yours. We will use [Nova Admin](https://github.com/novaframework/nova_admin) as an example. This is a nova application that lets you do observer kind of functionality.

In our nova application we open `rebar.config` and add Nova admin as a dependency.

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"}, %% Used for logging - Change if needed
        {nova_admin, ".*", {git, "git@github.com:novaframework/nova_admin.git", {branch, "master"}}}
       ]}.
```