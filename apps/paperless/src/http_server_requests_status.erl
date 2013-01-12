-module(http_server_requests_status).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(STATUS, <<"{\"status\": \"~s\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    Status = atom_to_list(scanner_server:status()),
    {ok, Reply} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/json">>}],
                                        ?TEMPLATE:render(?STATUS, [Status]), Req),
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
