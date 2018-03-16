-module(nvr).
-behaviour(gen_server).
-export([
          start_link/1,
          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3
        ]).

-include("wa.hrl").

%
% external
%

start_link(Params) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(Params) ->
  NewParams = [exec_me(P#{ pid => undefined, os_pid => undefined }) || P <- Params],
  {ok, NewParams}.

%
% gen_server
%

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _OsPid, process, Pid, _Result}, State) ->
  NewState = lists:map(fun
      (#{ pid := PPid } = P) when PPid =:= Pid -> exec_me(P#{ pid := undefined, os_pid := undefined });
      (Any) -> Any
    end, State),
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%
% local
%

exec_me(#{ type := record, cmd := Prefix } = P) ->
  Cmd = lists:concat([Prefix, utils:time_str(), ".mp4"]),
  exec_call(Cmd, P);
exec_me(#{ cmd := Cmd } = P) ->
  exec_call(Cmd, P).

exec_call(Cmd, P) ->
  case exec:run(Cmd, [monitor]) of
    {ok, Pid, OsPid} ->
      P#{ pid := Pid, os_pid := OsPid };
    Error ->
      ?ERROR("Can't start process ~p - ~p", [Cmd, Error]),
      P
  end.
