-module(http_server_template).

-export([render/2]).

render(Template, Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).
