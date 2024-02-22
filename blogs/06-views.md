## Views

Nova is using `ErlyDTL` to tempalte the views, that is an Erlang version of Django templating.

You can read more about Django templating [here](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html).

### Controller responses for views

**Simple interface**
Keyword: view

Spec: `{view, Variables :: map() | [{Key :: atom() | binary() | string(), Value :: any()}]}`

Example: `{view, #{my_var => "123"}}`

Renders the corresponding view with the variables attached. If a controller is named `my_simple_controller.erl` the view is named `my_simple_view.dtl`.

**Advanced interface**

Spec: `{view, Variables :: map() | [{Key :: atom() | binary() | string(), Value :: any()}], Options :: map()}`

Example: `{view, #{my_var => "123"}, #{view => my_view_mod}}`

Same as the simple interface but where you can define some options. Currently the only option for this interface is view which enables the user to specify a view other than the corresponding one based on controllers name. In the example above the dtl-file my_view_mod.dtl would be rendered.

