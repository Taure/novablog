---
title: "Database Integration"
weight: 1
---
## Database Integration

Nova is a web framework and does not include a built-in ORM or database layer. This is by design, you are free to use whatever database and driver fits your project. In this article we will look at how to integrate PostgreSQL using `pgo` and structure our data access code.

### Why pgo?

[pgo](https://github.com/erleans/pgo) is a PostgreSQL client for Erlang with built-in connection pooling. Unlike some other drivers, you don't need to manage connections yourself or add a separate pool library like `poolboy`. You configure a pool in your sys.config and just call `pgo:query/2` — pgo handles checkout, checkin and reconnection for you.

### Adding the dependency

Add `pgo` to `rebar.config`:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        pgo
       ]}.
```

Also add `pgo` to your application dependencies in `src/my_first_nova.app.src`:

```erlang
{application, my_first_nova,
 [{description, "My first Nova application"},
  {vsn, git},
  {registered, []},
  {mod, {my_first_nova_app, []}},
  {applications,
   [kernel,
    stdlib,
    nova,
    pgo
   ]},
  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
```

### Database configuration

pgo pools are configured in `sys.config` under the `pgo` application key. Add this to your `dev_sys.config.src`:

```erlang
{pgo, [
    {pools, [
        {default, #{
            pool_size => 10,
            host => "localhost",
            port => 5432,
            database => "my_first_nova_dev",
            user => "postgres",
            password => "postgres"
        }}
    ]}
]}
```

The `default` pool is what `pgo:query/2` uses when you don't specify a pool name. The `pool_size` controls how many connections pgo keeps open.

That is it for setup. No gen_server needed, no connection manager, no supervision tree changes. pgo starts its own pool supervisor when the application boots.

### Creating a repository module

Now let's create a module that handles our data operations. Create `src/my_first_nova_user_repo.erl`:

```erlang
-module(my_first_nova_user_repo).
-export([
         all/0,
         get/1,
         create/2,
         update/3,
         delete/1
        ]).

all() ->
    case pgo:query("SELECT id, name, email FROM users ORDER BY id") of
        #{rows := Rows} ->
            {ok, [row_to_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

get(Id) ->
    case pgo:query("SELECT id, name, email FROM users WHERE id = $1", [Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

create(Name, Email) ->
    case pgo:query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                   [Name, Email]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Name, Email) ->
    case pgo:query("UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email",
                   [Name, Email, Id]) of
        #{rows := [Row]} ->
            {ok, row_to_map(Row)};
        #{rows := []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

delete(Id) ->
    case pgo:query("DELETE FROM users WHERE id = $1", [Id]) of
        #{command := delete, num_rows := 1} -> ok;
        #{num_rows := 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

row_to_map({Id, Name, Email}) ->
    #{id => Id, name => Name, email => Email}.
```

Notice how clean this is compared to managing connections manually. `pgo:query/1` and `pgo:query/2` handle the connection pooling transparently. The result is a map with a `rows` key containing tuples.

### Setting up the database

Before we can use our repo, we need to create the database and table. You can do this with psql:

```sql
CREATE DATABASE my_first_nova_dev;

\c my_first_nova_dev

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    inserted_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);
```

### Using the repo in controllers

Now we can update our API controller from the previous article to use real data instead of hardcoded values:

```erlang
-module(my_first_nova_api_controller).
-export([
         index/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

index(_Req) ->
    {ok, Users} = my_first_nova_user_repo:all(),
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_user_repo:get(binary_to_integer(Id)) of
        {ok, User} ->
            {json, User};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.

create(#{params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    case my_first_nova_user_repo:create(Name, Email) of
        {ok, User} ->
            {json, 201, #{}, User};
        {error, Reason} ->
            {status, 422, #{}, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.

update(#{bindings := #{<<"id">> := Id},
         params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    case my_first_nova_user_repo:update(binary_to_integer(Id), Name, Email) of
        {ok, User} ->
            {json, User};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_user_repo:delete(binary_to_integer(Id)) of
        ok ->
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"user not found">>}}
    end.
```

Don't forget to add the new routes for update and delete:

```erlang
#{prefix => "/api",
  security => false,
  routes => [
             {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
             {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
             {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}},
             {"/users/:id", fun my_first_nova_api_controller:update/1, #{methods => [put]}},
             {"/users/:id", fun my_first_nova_api_controller:delete/1, #{methods => [delete]}}
            ]
}
```

### Named pools

If you need multiple databases or want separate pools for different workloads, you can configure named pools:

```erlang
{pgo, [
    {pools, [
        {default, #{
            pool_size => 10,
            host => "localhost",
            database => "my_first_nova_dev",
            user => "postgres",
            password => "postgres"
        }},
        {readonly, #{
            pool_size => 5,
            host => "localhost",
            database => "my_first_nova_dev",
            user => "readonly_user",
            password => "readonly_pass"
        }}
    ]}
]}
```

Then query a specific pool:

```erlang
pgo:query(readonly, "SELECT count(*) FROM users", []).
```

### Transactions

pgo supports transactions:

```erlang
pgo:transaction(fun() ->
    pgo:query("INSERT INTO users (name, email) VALUES ($1, $2)", [Name, Email]),
    pgo:query("INSERT INTO audit_log (action, target) VALUES ($1, $2)", [<<"create_user">>, Email])
end).
```

If any query inside the transaction fails, the whole thing is rolled back.

### Other databases

The same repository pattern works for any database. Some popular Erlang database drivers:

- **PostgreSQL**: `pgo`, `epgsql`
- **MySQL**: `mysql-otp`
- **Redis**: `eredis`
- **Mnesia**: Built into OTP, no external dependency needed
- **SQLite**: `esqlite`

The key idea is the same: configure your driver, write repository modules for your data operations, and call them from your controllers.

In the next article we will look at [Kura](../kura/), an Ecto-inspired database layer that builds on pgo to give you schemas, changesets, automatic migrations and a query builder.
