-module(http_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(OK, <<"ok">>).
-define(TEMPLATE_STATUS, "{\"status\": \"~s\"}").
%% API
-export([start_link/1, dispatch_requests/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  mochiweb_http:start([{port, Port},
               {loop, fun dispatch_requests/1}]),
  erlang:monitor(process, mochiweb_http),
  {ok, []}.

stop() ->
  gen_server:cast(?SERVER, stop).

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _, _, {mochiweb_http, _}, _}, State) ->
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  mochiweb_http:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
handle("/status", Req) ->
  Res = render_template(?TEMPLATE_STATUS, [scanner_server:status()]),
  respond_success(Req, Res);
handle(_, Req) ->
  respond_error(Req, <<"{\"error\": \"unknown command\"}">>).



respond_error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/json"}], Body}).

respond_success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/json"}], Body}).

render_template(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.
