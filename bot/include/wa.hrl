%
% Common project options
%

-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).
-define(ASYNC(F), proc_lib:spawn(fun() -> F end)).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 50, Type, [I]}).
-define(CHILD(I, Type, Param), {I, {I, start_link, Param}, permanent, 50, Type, [I]}).
-define(CHILD(Id, I, Type, Param), {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

%
% Configuration
%

-define(CONFIG(Key, Default), application:get_env(wa, Key, Default)).
-define(PV(Key, Set), proplists:get_value(Key, Set)).
-define(PV(Key, Set, Default), proplists:get_value(Key, Set, Default)).

%
% Logger
%

-define(ERROR(Msg), lager:error(Msg, [])).
-define(ERROR(Msg, Params), lager:error(Msg, Params)).
-define(INFO(Msg), lager:info(Msg, [])).
-define(INFO(Msg, Params), lager:info(Msg, Params)).
-define(WARNING(Msg), lager:warning(Msg, [])).
-define(WARNING(Msg, Params), lager:warning(Msg, Params)).
-define(DEBUG(Msg), lager:debug(Msg, [])).
-define(DEBUG(Msg, Params), lager:debug(Msg, Params)).

%
% Timing
%

-define(S2MS(S), S * 1000).
-define(RECONNECT_TIMEOUT, ?S2MS(5)).
-define(GUN_TIMEOUT, ?S2MS(60)).
