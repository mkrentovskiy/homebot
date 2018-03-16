-module(voice_varvara).
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

-include("include/wa.hrl").


start_link(Params) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

%
% gen_server
%

init(Params) ->
  ?AFTER(0, init),
  {ok, Params}.

handle_call(Msg, _From, State) ->
  ?INFO("Unknown handle_call ~p for state ~p", [Msg, State]),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  ?INFO("Unknown handle_cast ~p for state ~p", [Msg, State]),
  {noreply, State}.

handle_info(init, #{ cmd := Cmd, dir := Dir } = State) ->
  {ok, _, _} = exec:run(Cmd, [stdout, {cd, Dir}]),
  {noreply, State};
handle_info({stdout, _, Phrase}, State) ->
  ?INFO("Users say - ~ts", [Phrase]),
  parse(binary:split(binary:replace(Phrase, <<"\n">>, <<>>, [global]), <<" ">>, [global])),
  {noreply, State};
handle_info(Msg, State) ->
  ?INFO("Unknown handle_info ~p for state ~p", [Msg, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%
% local
%

parse([_, <<"включи"/utf8>>, <<"свет"/utf8>>]) ->
  operations:light(undefined, "FFFFFF");
parse([_, <<"выключи"/utf8>>, <<"свет"/utf8>>]) ->
  operations:light(undefined, "000000");  
parse(R) ->
  ?INFO("Unknown command - ~p", [R]).
