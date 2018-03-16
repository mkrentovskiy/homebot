-module(wa_sup).
-behaviour(supervisor).
-export([
          start_link/0,
          init/1
        ]).

-include("wa.hrl").


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Modules = lists:flatten([apply(M) || M <- ?CONFIG(modules, [])]),
  {ok, {{one_for_one, 50, 100}, Modules}}.

apply({Module, ConfigName}) ->
  ?CHILD(Module, worker, [ConfigName]);
apply(Module) ->
  ?CHILD(Module, worker).
