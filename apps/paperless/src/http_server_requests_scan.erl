-module(http_server_requests_scan).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(OK, <<"{\"status\": \"ok\"}~n">>).
-define(BAD_REQUEST, <<"{\"error\": \"bad request\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Reply} =
        case {scanner_server:status(), Method} of
            {ready, <<"POST">>} ->
                scanner_server:scan(),
                cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/x-json">>}],
                                 ?TEMPLATE:render(?OK, []), Req);
            _ ->
                cowboy_req:reply(405, [{<<"Content-Type">>, <<"application/x-json">>}],
                                 ?TEMPLATE:render(?BAD_REQUEST, []), Req)
        end,
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
