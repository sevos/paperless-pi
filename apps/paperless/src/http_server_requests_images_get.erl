-module(http_server_requests_images_get).

-export([init/3, handle/2, terminate/2]).

-define(TEMPLATE, http_server_template).
-define(NOT_FOUND_JSON, <<"{\"image_not_found\": \"~w\"}~n">>).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, []}.

handle(Req, State) ->
    {RawImageId, _} = cowboy_req:binding(id, Req),
    ImageId = list_to_integer(binary_to_list(RawImageId)),
    {ok, Reply} =
      case image_store:get(ImageId) of
        {error, not_found} ->
          cowboy_req:reply(404, [{<<"Content-Type">>, <<"text/json">>}],
                           ?TEMPLATE:render(?NOT_FOUND_JSON, [ImageId]), Req);
        {ok, Image} ->
          cowboy_req:reply(200, [{<<"Content-Type">>, <<"image/jpeg">>}], Image, Req)
      end,
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.
