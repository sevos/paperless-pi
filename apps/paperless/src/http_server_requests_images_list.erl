-module(http_server_requests_images_list).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(LIST_JSON, <<"{\"available_images\": \"~p\"}~n">>).
-define(OK, <<"{\"status\": \"ok\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Reply} =
        case Method of
            <<"GET">> ->
                Images = image_store:list(),
                cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/x-json">>}],
				 ?TEMPLATE:render(?LIST_JSON, [Images]), Req);
            <<"DELETE">> ->
                image_store:flush(),
                cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/json">>}],
				 ?TEMPLATE:render(?OK, []), Req)
        end,
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
