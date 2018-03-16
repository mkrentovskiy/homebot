-module(cleaner).
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
-include_lib("kernel/include/file.hrl").

%
% external
%

start_link(Params) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).


init(#{ timeout := T } = Params) ->
  ?AFTER(?S2MS(T), cleanup),
  {ok, Params}.

%
% gen_server
%

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(cleanup, #{ timeout := T, folders := F } = State) ->
  [cleanup(I, State) || I <- F],
  ?AFTER(?S2MS(T), cleanup),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%
% local
%

cleanup(Folder, #{ max_size := MaxSize }) ->
  case file:list_dir(Folder) of
    {ok, Files} ->
      Sorted = lists:sort(
          fun({_, A, _}, {_, B, _}) -> A > B end, 
          lists:foldl(fun(F, A) ->  
              FF = filename:join([Folder, F]),
              case file:read_file_info(FF) of
                {ok, FInfo} ->
                  A ++ [{FF, calendar:datetime_to_gregorian_seconds(FInfo#file_info.mtime), FInfo#file_info.size}];
                Error ->
                  ?ERROR("Can't read file info for ~p - ~p", [FF, Error]),
                  A
              end
            end, [], Files)),
      lists:foldl(fun({FF, _, Size}, Total) -> 
          case Total + Size of
            N when N > MaxSize ->
              ?INFO("Cleanup - remove ~p", [FF]),
              os:cmd("rm -rf " ++ FF),
              Total;
            N ->
              N
          end
        end, 0, Sorted);
    Error ->
      ?ERROR("Can't read folder ~p - ~p", [Folder, Error]),
      ok
  end.
