-module(image_store).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(image,
    {id,
     body}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([store/1]).
-export([get/1]).
-export([list/0]).
-export([flush/0]).
-export([stop/0]).

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
store(Body) ->
    gen_server:cast(?SERVER, {store, Body}).
get(Id) ->
    gen_server:call(?SERVER, {get, Id}).
list() ->
    gen_server:call(?SERVER, list).
flush() ->
    gen_server:cast(?SERVER, flush).
stop() ->
    gen_server:cast(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, []}.

handle_cast({store, Body}, Store) ->
    {noreply, [#image{id=next_id(Store), body=Body}|Store]};
handle_cast(flush, _Store) ->
    {noreply, []};
handle_cast(stop, _Store) ->
    {stop, normal, []};
handle_cast(_, Status) ->
    {noreply, Status}.

handle_call(list, _From, Store) ->
    {reply, [Image#image.id || Image <- Store], Store};
handle_call({get, Id}, _From, Store) ->
    Results = [Image#image.body || Image <- Store, Image#image.id == Id],
    Reply = case Results of
        [Body] -> {ok, Body};
        [] -> {error, not_found}
    end,
    {reply, Reply, Store};
handle_call(_, _From, Status) ->
    {reply, ok, Status}.

handle_info(_Payload, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

next_id(Store) ->
    case Store of
        [] -> 0;
        _  -> lists:max([Image#image.id || Image <- Store]) + 1
    end.
