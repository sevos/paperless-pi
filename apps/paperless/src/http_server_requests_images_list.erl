-module(http_server_requests_images_list).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(LIST_JSON, <<"{\"available_images\": \"~p\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    Images = image_store:list(),
    {ok, Reply} = cowboy_req:reply(404, [{<<"Content-Type">>, <<"text/json">>}],
                                        ?TEMPLATE:render(?LIST_JSON, [Images]), Req),
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
