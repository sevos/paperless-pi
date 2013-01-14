-module(scanner_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/0]).
-export([scan/0]).
-export([status/0]).

                                                % Internal workers
-export([scan_image_from_default_scanner/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
scan() ->
    gen_server:cast(?SERVER, scan).
status() ->
    gen_server:call(?SERVER, status).
reset() ->
    gen_server:cast(?SERVER, reset).
stop() ->
    gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, ready}.

handle_cast(scan, ready) ->
    Worker = spawn(scanner_server, scan_image_from_default_scanner, [self()]),
    {noreply, busy};
handle_cast(scan, State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State }.

handle_call(status, _From, Status) ->
    {reply, Status, Status}.

handle_info(Payload, State) ->
    case Payload of
        {worker_finished, Data} ->
            image_store:store(Data),
            {noreply, ready};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

scan_image_from_default_scanner(Caller) ->
    os:cmd("sudo scanimage | pnmtopng > tmp.jpg"),
    {ok, Image} = file:read_file("tmp.jpg"),
    file:delete("tmp.jpg"),
    Caller ! {worker_finished, Image}.
