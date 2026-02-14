---
title: "Kura - Database Layer"
weight: 2
---
## Kura - Database Layer

In the previous article we used `pgo` directly to query PostgreSQL. That works well, but as your application grows you end up writing a lot of boilerplate: manual SQL strings, row-to-map conversions, parameter lists. [Kura](https://github.com/Taure/kura) is an Ecto-inspired database layer for Erlang that gives you schemas, changesets, a query builder, automatic migrations and a repository pattern — all on top of pgo.

### Why kura?

Compare creating a user with raw pgo:

```erlang
create(Name, Email) ->
    case pgo:query("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                   [Name, Email]) of
        #{rows := [Row]} -> {ok, row_to_map(Row)};
        {error, Reason}  -> {error, Reason}
    end.
```

With kura:

```erlang
create(Params) ->
    CS = kura_changeset:cast(user, #{}, Params, [name, email]),
    CS1 = kura_changeset:validate_required(CS, [name, email]),
    my_first_nova_repo:insert(CS1).
```

No SQL strings, no positional parameters, no manual row conversion. Kura also gives you input validation through changesets, so invalid data is caught before it hits the database.

### Setup

#### Docker Compose

First, set up PostgreSQL. Create a `docker-compose.yml` in your project root:

```yaml
services:
  postgres:
    image: postgres:16
    ports:
      - "5432:5432"
    environment:
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: my_first_nova_dev
    volumes:
      - pgdata:/var/lib/postgresql/data
volumes:
  pgdata:
```

Start it with:

```shell
$ docker compose up -d
```

#### Dependencies

Add `kura` as a dependency and `rebar3_kura` as a project plugin in `rebar.config`:

```erlang
{deps, [
    nova,
    {flatlog, "0.1.2"},
    {kura, "~> 0.3"}
]}.

{project_plugins, [
    {rebar3_kura, "~> 0.3"},
    erlfmt
]}.
```

The `rebar3_kura` plugin adds a compile hook that diffs your schemas against existing migrations and auto-generates new migration files when something changes. Add it to your provider hooks:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {erlydtl, compile}},
        {compile, {kura, compile}}
    ]}
]}.
```

Since schemas live in `src/schemas/` and migrations in `src/migrations/`, make sure your source dirs are set to recursive:

```erlang
{src_dirs, [{"src", [{recursive, true}]}]}.
```

Finally, add `kura` to your application dependencies in `src/my_first_nova.app.src`:

```erlang
{applications, [
    kernel,
    stdlib,
    nova,
    kura
]}
```

#### Repository module

The repo module is the interface between your application and the database. Create `src/my_first_nova_repo.erl`:

```erlang
-module(my_first_nova_repo).
-behaviour(kura_repo).

-export([
    config/0,
    start/0,
    all/1,
    get/2,
    get_by/2,
    one/1,
    insert/1,
    insert/2,
    update/1,
    delete/1,
    insert_all/2,
    update_all/2,
    delete_all/1,
    preload/3,
    transaction/1,
    multi/1,
    query/2
]).

config() ->
    Database = application:get_env(my_first_nova, database, <<"my_first_nova_dev">>),
    #{
        pool => ?MODULE,
        database => Database,
        hostname => <<"localhost">>,
        port => 5432,
        username => <<"postgres">>,
        password => <<"postgres">>,
        pool_size => 10
    }.

start() -> kura_repo_worker:start(?MODULE).
all(Q) -> kura_repo_worker:all(?MODULE, Q).
get(Schema, Id) -> kura_repo_worker:get(?MODULE, Schema, Id).
get_by(Schema, Clauses) -> kura_repo_worker:get_by(?MODULE, Schema, Clauses).
one(Q) -> kura_repo_worker:one(?MODULE, Q).
insert(CS) -> kura_repo_worker:insert(?MODULE, CS).
insert(CS, Opts) -> kura_repo_worker:insert(?MODULE, CS, Opts).
update(CS) -> kura_repo_worker:update(?MODULE, CS).
delete(CS) -> kura_repo_worker:delete(?MODULE, CS).
insert_all(Schema, Entries) -> kura_repo_worker:insert_all(?MODULE, Schema, Entries).
update_all(Q, Updates) -> kura_repo_worker:update_all(?MODULE, Q, Updates).
delete_all(Q) -> kura_repo_worker:delete_all(?MODULE, Q).
preload(Schema, Records, Assocs) -> kura_repo_worker:preload(?MODULE, Schema, Records, Assocs).
transaction(Fun) -> kura_repo_worker:transaction(?MODULE, Fun).
multi(Multi) -> kura_repo_worker:multi(?MODULE, Multi).
query(SQL, Params) -> kura_repo_worker:query(?MODULE, SQL, Params).
```

The `config/0` callback tells kura how to connect to PostgreSQL. The database name is read from application config so it can differ between environments. Each function delegates to `kura_repo_worker` — this is the standard pattern.

Add the database config to your `dev_sys.config.src`:

```erlang
{my_first_nova, [
    {database, <<"my_first_nova_dev">>}
]}
```

### Schemas

Schemas define the shape of your data. They map Erlang atoms to database columns and tell kura which fields exist, their types and constraints. Create `src/schemas/product.erl`:

```erlang
-module(product).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0]).

table() -> <<"products">>.
primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = price, type = integer, nullable = false},
        #kura_field{name = description, type = text},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].
```

And `src/schemas/user.erl`:

```erlang
-module(user).
-behaviour(kura_schema).
-include_lib("kura/include/kura.hrl").

-export([table/0, fields/0, primary_key/0]).

table() -> <<"users">>.
primary_key() -> id.

fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = name, type = string, nullable = false},
        #kura_field{name = email, type = string, nullable = false},
        #kura_field{name = inserted_at, type = utc_datetime},
        #kura_field{name = updated_at, type = utc_datetime}
    ].
```

A schema module implements the `kura_schema` behaviour with three callbacks:

- `table/0` — the PostgreSQL table name
- `primary_key/0` — the primary key field
- `fields/0` — a list of `#kura_field{}` records defining columns

Common field types: `id`, `string`, `text`, `integer`, `float`, `boolean`, `utc_datetime`, `date`.

### Automatic migrations

When you compile, `rebar3_kura` compares your schema modules against existing migrations and generates new migration files if anything changed:

```shell
$ rebar3 compile
===> kura: generated migration m20260214182218_update_schema
===> Compiling my_first_nova
```

The generated migration creates both tables:

```erlang
-module(m20260214182218_update_schema).
-behaviour(kura_migration).
-include_lib("kura/include/kura.hrl").
-export([up/0, down/0]).

up() ->
    [{create_table, <<"products">>, [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = name, type = string, nullable = false},
        #kura_column{name = price, type = integer, nullable = false},
        #kura_column{name = description, type = text},
        #kura_column{name = inserted_at, type = utc_datetime},
        #kura_column{name = updated_at, type = utc_datetime}
    ]},
     {create_table, <<"users">>, [
        #kura_column{name = id, type = id, primary_key = true, nullable = false},
        #kura_column{name = name, type = string, nullable = false},
        #kura_column{name = email, type = string, nullable = false},
        #kura_column{name = inserted_at, type = utc_datetime},
        #kura_column{name = updated_at, type = utc_datetime}
    ]}].

down() ->
    [{drop_table, <<"products">>},
     {drop_table, <<"users">>}].
```

You never write migrations by hand. Change a schema, recompile, and a new migration appears. If you later add a `category` field to the product schema, the next compile generates a migration with an `{add_column, ...}` operation.

### Application startup

Start the repo and run pending migrations when the application boots. Update `src/my_first_nova_app.erl`:

```erlang
start(_StartType, _StartArgs) ->
    my_first_nova_repo:start(),
    kura_migrator:migrate(my_first_nova_repo),
    my_first_nova_sup:start_link().
```

`my_first_nova_repo:start/0` starts the connection pool. `kura_migrator:migrate/1` applies any pending migrations. This means your database schema is always up to date when the application starts — no manual migration step needed.

### Using kura in controllers

Now let's build a products API controller that uses kura for all data operations.

#### Listing records

```erlang
list(_Req) ->
    {ok, Products} = my_first_nova_repo:all(kura_query:from(product)),
    {json, #{products => Products}}.
```

`kura_query:from(product)` creates a query that selects all records from the products table. The repo returns them as maps with atom keys.

#### Fetching a single record

```erlang
show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Product} ->
            {json, Product};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end.
```

`get/2` looks up a record by primary key and returns `{ok, Map}` or `{error, not_found}`.

#### Creating with changesets

```erlang
create(#{json := Params}) ->
    CS = kura_changeset:cast(product, #{}, Params, [name, price, description]),
    CS1 = kura_changeset:validate_required(CS, [name, price]),
    case my_first_nova_repo:insert(CS1) of
        {ok, Product} ->
            {json, 201, #{}, Product};
        {error, Changeset} ->
            {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
    end.
```

This is where kura really shines. `kura_changeset:cast/4` takes:

1. The schema module (`product`)
2. The existing data (empty map `#{}` for new records)
3. The incoming parameters (from the JSON request body)
4. The list of fields to accept (allowlist)

Only the listed fields are accepted — everything else is ignored. Then `validate_required/2` ensures `name` and `price` are present. If validation fails, `insert/1` returns `{error, Changeset}` with the errors, which we send back as a 422 response.

#### Updating

```erlang
update(#{bindings := #{<<"id">> := Id}, json := Params}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Existing} ->
            CS = kura_changeset:cast(product, Existing, Params, [name, price, description]),
            case my_first_nova_repo:update(CS) of
                {ok, Updated} ->
                    {json, Updated};
                {error, Changeset} ->
                    {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
            end;
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end.
```

For updates, we first fetch the existing record, then cast the incoming params onto it. Only the fields in the allowlist are updated — the rest stay unchanged.

#### Deleting

```erlang
delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Record} ->
            CS = kura_changeset:cast(product, Record, #{}, []),
            {ok, _} = my_first_nova_repo:delete(CS),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end.
```

Delete requires a changeset wrapping the record. We cast with an empty params map and an empty field list since we don't need to change anything — just identify the record to delete.

### The complete controller

Here is the full products controller:

```erlang
-module(my_first_nova_products_controller).
-include_lib("kura/include/kura.hrl").
-export([list/1, show/1, create/1, update/1, delete/1]).

list(_Req) ->
    {ok, Products} = my_first_nova_repo:all(kura_query:from(product)),
    {json, #{products => Products}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Product} ->
            {json, Product};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{json := Params}) ->
    CS = kura_changeset:cast(product, #{}, Params, [name, price, description]),
    CS1 = kura_changeset:validate_required(CS, [name, price]),
    case my_first_nova_repo:insert(CS1) of
        {ok, Product} ->
            {json, 201, #{}, Product};
        {error, Changeset} ->
            {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and price required">>}}.

update(#{bindings := #{<<"id">> := Id}, json := Params}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Existing} ->
            CS = kura_changeset:cast(product, Existing, Params, [name, price, description]),
            case my_first_nova_repo:update(CS) of
                {ok, Updated} ->
                    {json, Updated};
                {error, Changeset} ->
                    {status, 422, #{}, #{errors => Changeset#kura_changeset.errors}}
            end;
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
update(_Req) ->
    {status, 422, #{}, #{error => <<"invalid request">>}}.

delete(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_repo:get(product, binary_to_integer(Id)) of
        {ok, Record} ->
            CS = kura_changeset:cast(product, Record, #{}, []),
            {ok, _} = my_first_nova_repo:delete(CS),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"not found">>}}
    end;
delete(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.
```

Note the `-include_lib("kura/include/kura.hrl")` at the top. This imports the `#kura_changeset{}` record so we can access `Changeset#kura_changeset.errors` in error responses.

### Query builder

So far we have used simple `kura_query:from(schema)` queries. The query builder supports filtering and ordering too:

```erlang
%% Filter by field
Q = kura_query:where(kura_query:from(product), #{name => <<"Widget">>}),
{ok, Products} = my_first_nova_repo:all(Q).

%% Order results
Q = kura_query:order_by(kura_query:from(product), {name, asc}),
{ok, Products} = my_first_nova_repo:all(Q).

%% Combine
Q = kura_query:order_by(
      kura_query:where(kura_query:from(product), #{price => 999}),
      {name, asc}),
{ok, Products} = my_first_nova_repo:all(Q).
```

For queries that return a single result, use `one/1` instead of `all/1`:

```erlang
Q = kura_query:where(kura_query:from(user), #{email => <<"alice@example.com">>}),
{ok, User} = my_first_nova_repo:one(Q).
```

### Transactions

Wrap multiple operations in a transaction:

```erlang
my_first_nova_repo:transaction(fun() ->
    CS1 = kura_changeset:cast(product, #{}, #{name => <<"Widget">>, price => 999}, [name, price]),
    {ok, _} = my_first_nova_repo:insert(CS1),
    CS2 = kura_changeset:cast(product, #{}, #{name => <<"Gadget">>, price => 1999}, [name, price]),
    {ok, _} = my_first_nova_repo:insert(CS2)
end).
```

If any operation inside the function fails, the entire transaction is rolled back.

### Bulk operations

Insert multiple records at once:

```erlang
Products = [
    #{name => <<"Widget">>, price => 999},
    #{name => <<"Gadget">>, price => 1999},
    #{name => <<"Doohickey">>, price => 499}
],
{ok, Inserted} = my_first_nova_repo:insert_all(product, Products).
```

### Testing it

Start PostgreSQL and the application:

```shell
$ docker compose up -d
$ rebar3 shell
```

Test with curl:

```shell
# List products (empty)
$ curl -s localhost:8080/api/products
{"products":[]}

# Create a product
$ curl -s -X POST localhost:8080/api/products \
  -H "Content-Type: application/json" \
  -d '{"name":"Widget","price":999}'

# List products
$ curl -s localhost:8080/api/products

# Get a product
$ curl -s localhost:8080/api/products/1

# Update a product
$ curl -s -X PUT localhost:8080/api/products/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Super Widget","price":1499}'

# Delete a product
$ curl -s -X DELETE localhost:8080/api/products/1
```

### Enum types

Kura supports enum fields that store as `VARCHAR(255)` in PostgreSQL and cast between atoms in Erlang and binaries in the database:

```erlang
fields() ->
    [
        #kura_field{name = id, type = id, primary_key = true, nullable = false},
        #kura_field{name = status, type = {enum, [draft, published, archived]}},
        #kura_field{name = title, type = string, nullable = false}
    ].
```

Casting accepts atoms, binaries, or charlists and validates against the allowed values:

```erlang
CS = kura_changeset:cast(post, #{}, #{status => <<"published">>}, [title, status]),
%% status is cast to the atom 'published'
```

No PostgreSQL `CREATE TYPE` is needed — the column is a plain `VARCHAR(255)`. Adding or removing enum values from your schema won't generate a new migration since the underlying column type doesn't change.

### Query telemetry

Enable query logging via `sys.config` under the `kura` application key:

```erlang
{kura, [
    {log, true}
]}.
```

This uses the built-in default logger (`logger:info`). You can also point to a custom handler:

```erlang
{kura, [
    {log, {my_first_nova_telemetry, handle_query}}
]}.
```

Each query emits an event map:

```erlang
#{query => <<"SELECT ...">>,
  params => [...],
  result => ok | error,
  num_rows => 3,
  duration_us => 1500,
  repo => my_first_nova_repo}
```

Use this for slow query logging, metrics collection, or debugging during development. If `log` is absent or `false`, there is no overhead.

### Nested changesets

Use `cast_assoc` to cast nested parameters into child changesets for `has_many` or `has_one` associations:

```erlang
Params = #{
    title => <<"My Post">>,
    body => <<"Hello world">>,
    comments => [
        #{body => <<"Great post!">>},
        #{body => <<"Thanks!">>}
    ]
},
CS = kura_changeset:cast(post, #{}, Params, [title, body]),
CS1 = kura_changeset:cast_assoc(CS, comments),
{ok, Post} = my_first_nova_repo:insert(CS1).
```

When `assoc_changes` are present, `insert` automatically wraps the operation in a transaction: the parent is inserted first, then each child gets the parent's primary key set as its foreign key.

You can provide a custom changeset function via the `with` option:

```erlang
CS1 = kura_changeset:cast_assoc(CS, comments, #{
    with => fun(Data, ChildParams) ->
        ChildCS = kura_changeset:cast(comment, Data, ChildParams, [body]),
        kura_changeset:validate_required(ChildCS, [body])
    end
}).
```

For programmatic association building (not from user input), use `put_assoc`:

```erlang
CS1 = kura_changeset:put_assoc(CS, comments, [
    #{body => <<"Auto-generated comment">>}
]).
```

### pgo vs kura

| | pgo | kura |
|---|---|---|
| SQL | Write it yourself | Generated from queries and schemas |
| Migrations | Manual SQL files | Auto-generated from schema diffs |
| Validation | In your controller | Changesets with composable validators |
| Result format | Tuples, manual conversion | Maps with atom keys |
| Setup | Minimal | Repo module + schemas |

Use pgo directly when you need raw SQL control or are working with a complex existing schema. Use kura when you want a structured data layer with schemas, validation and migrations managed for you.

In the next article we will look at how to test Nova applications, including how to mock kura repos for unit tests.
