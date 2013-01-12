-module(http_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).
-define(TEMPLATE_STATUS, "{\"status\": \"~s\"}").
%% API
-export([start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% API implementation
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
  gen_server:cast(?SERVER, stop).

%% gen_server implementation
init([Port]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  Dispatch = [
        {'_', [
                {[<<"status">>], http_server_requests_status, []},
                {'_', http_server_requests_catch_all, []}
              ]
        }
  ],
  {ok, CowboyPid} = cowboy:start_http(http_server_listener, 100, [{port, Port}], [
      {env, [{dispatch, Dispatch}]},
      {max_keepalive, 50},
      {timeout, 500}
  ]),
  erlang:monitor(process, CowboyPid),
  {ok, CowboyPid}.

handle_call(_,_, State) -> {reply, ok, State}.
handle_cast(_,State) -> {noreply, State}.
handle_info(Payload, CowboyPid) ->
    case Payload of
        {'DOWN', _, process, CowboyPid, Reason} ->
            {stop, Reason, []};
        _ ->
            {noreply, CowboyPid}
    end.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
