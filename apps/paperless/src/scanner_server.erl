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
-export([reset/0]).

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
    Worker = spawn(scanner_server, scan_image_from_default_scanner, self()),
    {noreply, {busy, Worker}};
handle_cast(scan, State) ->
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State };

handle_cast(reset, {completed, Data}) ->
    {noreply, ready};
handle_cast(reset, Status) ->
    {noreply, Status}.

handle_call(status, _From, Status) ->
    S = case Status of
            {X, _} -> X;
            X -> X
        end,
    {reply, S, Status}.

handle_info(Payload, State) ->
    case Payload of
        {worker_finished, Data} ->
            {noreply, {completed, Data}};
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
    Caller ! {worker_finished, os:cmd("sudo scanimage | pnmtopng")}.

