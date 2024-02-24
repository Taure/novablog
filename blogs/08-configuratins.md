## Configurations

One of the ideas we had with Nova was that it should be easy to configure it or add a module or libraries that you want to use. Example are that if you don't specify a json package Nova will use Thoas. But if you configure that you would like to use jsx for example it will use that so long you add it as a dependency in your application.

Nova have two layers of configurations, one is for Nova and how this application will work, the other configuration is for Cowboy the web server.


### Nova

Following parameters should be defined under the `nova`-key in your *sys.config*.

| Key | Description | Value |
|-----|-------------|-------|
| `use_persistent_term` | Use `persistent_term` module to store routing tree | `boolean()` |
| `use_stacktrace` | If Nova should include stacktrace in error-pages | `boolean()` |
| `render_error_pages` | If Nova should render error-pages for HTML-request | `boolean()` |
| `use_sessions` | Turn off/on support for sessions | `boolean()` |
| `session_manager` | Specify a module to use as the session manager. Defaults to `nova_session_ets` | `atom()` |
| `use_strict_routing`    | If the routing module should work under the strict mode. Using strict mode will cause errors if non-deterministic paths are detected. This is a beta-function so use with caution. | `boolean()` |
| `bootstrap_application` | Define which application to bootstrap with Nova. This should be the name of your application. | `atom()` |
| `cowboy_configuration` | If you need some additional configuration done to Cowboy this is the place. Check `nova_sup` module to learn which keys that can be defined. | `map()` |


### Cowboy

Nova uses Cowboy as the webserver. Cowboy is a very flexible webserver and Nova tries to expose as much of this flexibility as possible. This means that you can configure Cowboy in a lot of different ways. The configuration is done in the `nova`-application under `cowboy_configuration`-key in your *sys.config*.

| Key | Description | Value | Default |
|-----|-------------|-------|---------|
| `stream_handlers` | Stream handlers are used to handle streaming requests. You can configure multiple stream handlers. Read more in the subsection *Stream handlers* | `list()` | `[nova_stream_h, cowboy_compress_h, cowboy_stream_h]` |
| `options` | Cowboy options. Read more in the subsection *Cowboy options* | `map()` | `#{compress => true}` |
| `ip` | IP to bind to | `tuple` | `{0, 0, 0, 0}` |
| `port` | Port to bind to | `integer()` | `8080` |
| `use_ssl` | If SSL should be used | `boolean()` | `false` |
| `ssl_port` | Port to bind to when using SSL | `integer()` | `8443` |
| `ca_cert` | Path to CA-cert | `string()` | `undefined` |
| `cert` | Path to cert | `string()` | `undefined` |
