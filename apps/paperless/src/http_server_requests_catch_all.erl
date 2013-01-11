-module(http_server_requests_catch_all).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(JSON, <<"{\"not_found\": \"/~s\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Path,_} = cowboy_http_req:path(Req),
    {ok, Reply} = cowboy_http_req:reply(404, [{<<"Content-Type">>, <<"text/json">>}],
                                        ?TEMPLATE:render(?JSON, [Path]), Req),
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
