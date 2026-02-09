---
title: "Database Integration"
date: 2024-09-02T00:00:00+00:00
weight: 10
tags: ["nova", "erlang", "database", "postgresql"]
series: ["Nova Framework Guide"]
summary: "Integrating PostgreSQL with epgsql for data persistence."
---

## Database Integration

Nova is a web framework and does not include a built-in ORM or database layer. This is by design, you are free to use whatever database and driver fits your project. In this article we will look at how to integrate PostgreSQL using `epgsql` and structure our data access code.

### Adding the dependency

We will use `epgsql` as our PostgreSQL driver. Add it to `rebar.config`:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        {epgsql, "4.7.1"}
       ]}.
```

Also add `epgsql` to your application dependencies in `src/my_first_nova.app.src`:

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
    epgsql
   ]},
  {licenses, ["Apache-2.0"]},
  {links, []}
 ]}.
```

### Database configuration

Add your database configuration to `dev_sys.config.src`:

```erlang
{my_first_nova, [
    {db, #{
        host => "localhost",
        port => 5432,
        database => "my_first_nova_dev",
        username => "postgres",
        password => "postgres"
    }}
]}
```

### Connection management

We need a process to manage our database connection. Create `src/my_first_nova_db.erl`:

```erlang
-module(my_first_nova_db).
-behaviour(gen_server).

-export([start_link/0,
         get_connection/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_connection() ->
    gen_server:call(?MODULE, get_connection).

init([]) ->
    {ok, DbConfig} = application:get_env(my_first_nova, db),
    #{host := Host, port := Port, database := Database,
      username := Username, password := Password} = DbConfig,
    case epgsql:connect(Host, Username, Password,
                        #{database => Database, port => Port}) of
        {ok, Conn} ->
            {ok, #{conn => Conn}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_connection, _From, State = #{conn := Conn}) ->
    {reply, {ok, Conn}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
```

This is a simple gen_server that holds a single database connection. For a production application you would want to use a connection pool like `poolboy`, but this keeps things simple for learning.

### Adding to the supervision tree

We need to start our database process under the supervisor. Update `src/my_first_nova_sup.erl`:

```erlang
-module(my_first_nova_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{id => my_first_nova_db,
          start => {my_first_nova_db, start_link, []},
          restart => permanent,
          type => worker}
    ],
    {ok, {#{strategy => one_for_all,
            intensity => 0,
            period => 1}, ChildSpecs}}.
```

### Creating a repository module

Now let's create a module that handles our data operations. This is where we write our SQL queries. Create `src/my_first_nova_user_repo.erl`:

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
    {ok, Conn} = my_first_nova_db:get_connection(),
    case epgsql:equery(Conn, "SELECT id, name, email FROM users ORDER BY id", []) of
        {ok, _Columns, Rows} ->
            {ok, [row_to_map(Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

get(Id) ->
    {ok, Conn} = my_first_nova_db:get_connection(),
    case epgsql:equery(Conn, "SELECT id, name, email FROM users WHERE id = $1", [Id]) of
        {ok, _Columns, [Row]} ->
            {ok, row_to_map(Row)};
        {ok, _Columns, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

create(Name, Email) ->
    {ok, Conn} = my_first_nova_db:get_connection(),
    case epgsql:equery(Conn,
                       "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                       [Name, Email]) of
        {ok, 1, _Columns, [Row]} ->
            {ok, row_to_map(Row)};
        {error, Reason} ->
            {error, Reason}
    end.

update(Id, Name, Email) ->
    {ok, Conn} = my_first_nova_db:get_connection(),
    case epgsql:equery(Conn,
                       "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email",
                       [Name, Email, Id]) of
        {ok, 1, _Columns, [Row]} ->
            {ok, row_to_map(Row)};
        {ok, 0, _Columns, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

delete(Id) ->
    {ok, Conn} = my_first_nova_db:get_connection(),
    case epgsql:equery(Conn, "DELETE FROM users WHERE id = $1", [Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

row_to_map({Id, Name, Email}) ->
    #{id => Id, name => Name, email => Email}.
```

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

### Connection pooling

For production applications you should use connection pooling. A common choice is `poolboy`. Add it to your deps:

```erlang
{deps, [
        nova,
        {flatlog, "0.1.2"},
        {epgsql, "4.7.1"},
        {poolboy, "1.5.2"}
       ]}.
```

Then configure a pool in your sys.config and modify the db module to checkout/checkin connections from the pool instead of holding a single connection. We will keep using the simple approach for this series.

### Other databases

The same pattern works for any database. Some popular Erlang database drivers:

- **PostgreSQL**: `epgsql`, `pgo`
- **MySQL**: `mysql-otp`
- **Redis**: `eredis`
- **Mnesia**: Built into OTP, no external dependency needed
- **SQLite**: `esqlite`

The key idea is the same: create a connection manager, write repository modules for your data operations, and call them from your controllers.

In the next article we will look at how to test Nova applications.
