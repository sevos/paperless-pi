-module(paperless_app).
-behaviour(application).
-export([start/2, stop/1, start/0]).

start() ->
    start_app(paperless, permanent).

start(_StartType, _StartArgs) ->
    paperless_sup:start_link().

stop(_State) ->
    ok.

start_app(App, Type) ->
    start_app(App, Type, application:start(App, Type)).
start_app(App, Type, Status) ->
    case Status of
        ok             -> ok;
        {error, Error} -> handle_error(Error, App, Type)
    end.
handle_error({already_started, _}, _App, _Type) -> ok;
handle_error({not_started, Dep}, App, Type) ->
    ok = start_app(Dep, Type),
    ok = start_app(App, Type).
