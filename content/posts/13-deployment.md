---
title: "Deploying a Nova Application"
date: 2024-09-23T00:00:00+00:00
weight: 13
tags: ["nova", "erlang", "deployment", "otp", "releases"]
series: ["Nova Framework Guide"]
summary: "Building OTP releases, production config, systemd, and Docker deployment."
---

## Deploying a Nova Application

In development we have been using `rebar3 nova serve` which gives us hot-reloading and debug logging. For production we need to build a proper OTP release. In this article we will look at how releases work, production configuration, and how to deploy a Nova application.

### Releases

An OTP release is a self-contained package that includes your application, all dependencies, and optionally the Erlang runtime itself. This means you can deploy to a server that doesn't even have Erlang installed.

Rebar3 uses `relx` to build releases. If you look at the `rebar.config` that was generated when we created our Nova app, you will see the release configuration:

```erlang
{relx, [{release, {my_first_nova, "0.1.0"},
         [my_first_nova,
          sasl]},
        {dev_mode, true},
        {include_erts, false},
        {extended_start_script, true},
        {sys_config_src, "config/dev_sys.config.src"},
        {vm_args_src, "config/vm.args.src"}
       ]}.
```

This is the development release config. It uses `dev_mode` which means it symlinks to your source instead of copying files, and it does not include ERTS (the Erlang runtime).

### Production profile

We override these settings for production using a rebar3 profile. Add this to your `rebar.config` if it is not already there:

```erlang
{profiles, [
    {prod, [
        {relx, [
            {dev_mode, false},
            {include_erts, true},
            {sys_config_src, "config/prod_sys.config.src"},
            {vm_args_src, "config/vm.args.src"}
        ]}
    ]}
]}.
```

Key differences from development:
- `dev_mode` is `false` so files are copied into the release
- `include_erts` is `true` so the Erlang runtime is bundled
- We use `prod_sys.config.src` instead of `dev_sys.config.src`

### Production configuration

Let's look at `config/prod_sys.config.src`:

```erlang
[
  {kernel, [
    {logger_level, info},
    {logger, [
      {handler, default, logger_std_h,
        #{config => #{file => "log/erlang.log"},
          formatter => {flatlog, #{
            map_depth => 3,
            term_depth => 50,
            colored => false,
            template => ["[", level, "] ", msg, "\n"]
          }}}}
    ]}
  ]},
  {nova, [
         {use_stacktrace, false},
         {environment, prod},
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {dev_mode, false},
         {bootstrap_application, my_first_nova},
         {plugins, [
                    {pre_request, nova_request_plugin, #{
                        decode_json_body => true,
                        read_urlencoded_body => true
                    }}
                   ]}
        ]},
  {my_first_nova, [
      {db, #{
          host => "${DB_HOST}",
          port => 5432,
          database => "${DB_NAME}",
          username => "${DB_USER}",
          password => "${DB_PASSWORD}"
      }}
  ]}
].
```

A few things to note:
- Logger level is `info` instead of `debug`
- Logs go to a file instead of stdout
- `use_stacktrace` is `false` so we don't leak stack traces to users
- `dev_mode` is `false`
- We use environment variables for database credentials with the `${VAR}` syntax. Rebar3 will substitute these when building the release.

### VM arguments

The `config/vm.args.src` file controls the Erlang VM settings:

```
-sname 'my_first_nova'
-setcookie my_first_nova_cookie
+K true
+A30
```

For production you might want to change a few things:

```
-name my_first_nova@${HOSTNAME}
-setcookie ${RELEASE_COOKIE}
+K true
+A30
+sbwt very_long
+swt very_low
```

- `-name` instead of `-sname` for full node names (needed for clustering)
- Environment variables for the cookie
- `+sbwt` and `+swt` tune the scheduler for lower latency

### Building the release

Build a production release:

```shell
$ rebar3 as prod release
```

This creates the release in `_build/prod/rel/my_first_nova/`. You can start it with:

```shell
$ _build/prod/rel/my_first_nova/bin/my_first_nova foreground
```

Or as a daemon:

```shell
$ _build/prod/rel/my_first_nova/bin/my_first_nova daemon
```

Other useful commands:

```shell
# Check if the node is running
$ _build/prod/rel/my_first_nova/bin/my_first_nova ping

# Attach a remote shell to the running node
$ _build/prod/rel/my_first_nova/bin/my_first_nova remote_console

# Stop the node
$ _build/prod/rel/my_first_nova/bin/my_first_nova stop
```

### Building a tarball

For deployment to another machine, you can build a compressed archive:

```shell
$ rebar3 as prod tar
```

This creates `_build/prod/rel/my_first_nova/my_first_nova-0.1.0.tar.gz`. You can copy this to your server, extract it and run it. Since we set `include_erts` to `true`, the server does not need Erlang installed.

```shell
# On the server
$ mkdir -p /opt/my_first_nova
$ tar -xzf my_first_nova-0.1.0.tar.gz -C /opt/my_first_nova
$ /opt/my_first_nova/bin/my_first_nova daemon
```

### SSL/TLS

To enable HTTPS, configure SSL in your Nova settings:

```erlang
{nova, [
    {cowboy_configuration, #{
        use_ssl => true,
        ssl_port => 8443,
        ssl_options => #{
            certfile => "/etc/letsencrypt/live/myapp.com/fullchain.pem",
            keyfile => "/etc/letsencrypt/live/myapp.com/privkey.pem"
        }
    }}
]}
```

Alternatively, you can put a reverse proxy like Nginx in front of your Nova application and let it handle SSL termination. This is the more common approach.

### Systemd service

To run your Nova application as a system service, create a systemd unit file:

```ini
[Unit]
Description=My First Nova Application
After=network.target postgresql.service

[Service]
Type=forking
User=nova
Group=nova
WorkingDirectory=/opt/my_first_nova
ExecStart=/opt/my_first_nova/bin/my_first_nova daemon
ExecStop=/opt/my_first_nova/bin/my_first_nova stop
Restart=on-failure
RestartSec=5
Environment=DB_HOST=localhost
Environment=DB_NAME=my_first_nova_prod
Environment=DB_USER=nova
Environment=DB_PASSWORD=secret
Environment=RELEASE_COOKIE=my_secret_cookie

[Install]
WantedBy=multi-user.target
```

Save this as `/etc/systemd/system/my_first_nova.service` and enable it:

```shell
$ sudo systemctl daemon-reload
$ sudo systemctl enable my_first_nova
$ sudo systemctl start my_first_nova
```

### Docker

You can also containerize your application. A simple Dockerfile:

```dockerfile
FROM erlang:26 AS builder

WORKDIR /app
COPY . .

RUN rebar3 as prod tar

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y libssl3 libncurses6 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/_build/prod/rel/my_first_nova/*.tar.gz .
RUN tar -xzf *.tar.gz && rm *.tar.gz

EXPOSE 8080

CMD ["/app/bin/my_first_nova", "foreground"]
```

Build and run:

```shell
$ docker build -t my_first_nova .
$ docker run -p 8080:8080 \
  -e DB_HOST=host.docker.internal \
  -e DB_NAME=my_first_nova_prod \
  -e DB_USER=nova \
  -e DB_PASSWORD=secret \
  my_first_nova
```

### Summary

Deploying a Nova application follows standard OTP release practices:

1. Configure a production profile in `rebar.config`
2. Set up production config with proper logging and secrets
3. Build a release with `rebar3 as prod release` or a tarball with `rebar3 as prod tar`
4. Deploy using systemd, Docker, or any other process manager

The nice thing about OTP releases is that they are self-contained. Once built, you have everything you need in a single directory or archive.

In the next and final article we will look at Nova's pub/sub system for real-time features.
