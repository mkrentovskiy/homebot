-module(wa_app).
-behaviour(application).

-export([
    start/2, 
    stop/1,
    priv_dir/0
]).

-include("wa.hrl").


start(_StartType, _StartArgs) ->
  application:ensure_all_started(lager),
  wa_sup:start_link().

stop(_State) ->
  ok.


root_dir() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  filename:dirname(Ebin).

priv_dir() ->
  filename:join(root_dir(), "priv").
