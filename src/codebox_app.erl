-module(codebox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = ensure_started(inets),
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(lhttpc),
    codebox_sup:start_link().

stop(_State) ->
    ok.


%% @doc Ensure that the application is started
-spec ensure_started(App) -> ok | {error, already_started, App} when
      App :: atom().
ensure_started(App) ->
    case application:start(App) of 
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
     end.
