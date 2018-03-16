-module(operations).
-behaviour(gen_server).

-export([
          cam/1,
          light/2,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include_lib("wa.hrl").

cam(ChatId) ->
  gen_server:cast(?MODULE, {cam, {ChatId, shot}}).

light(ChatId, RGB) ->
  gen_server:cast(?MODULE, {light, {ChatId, RGB}}).

%
% gen_server
%

start_link(Params) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).
init(Params) ->
  {ok, Params}.


handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast({Cmd, Params}, State) -> 
  {noreply, process(Cmd, Params, maps:get(Cmd, State), State)};
handle_cast(Msg, State) -> 
  ?INFO("Unknown cast - ~p", [Msg]),
  {noreply, State}.

handle_info(Info, State) ->
  ?INFO("Unknown info - ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

process(light, {ChatId, RGB}, #{ method := http, url := Prefix }, State) ->
  URL = lists:concat([Prefix, RGB]),
  case rest:request(get, plain, URL, [200], #{}, #{}) of
    {ok, _, _, _Reply} -> 
      telegram_bot:text(ChatId, <<"Настройка произведена"/utf8>>);
    Error -> 
      ?ERROR("Got error on light ~p - ~p", [URL, Error])
  end,
  State;
process(cam, {ChatId, shot}, #{ cmd := Cmd, image := Filename, filename := Prefix }, State) ->
  ?ASYNC(cam_shot(ChatId, Cmd, Filename, Prefix)),
  State;
process(Cmd, Params, _Options, State) ->
  ?INFO("Process ~p : ~p", [Cmd, Params]),
  State.

cam_shot(ChatId, Cmd, Filename, Prefix) ->
  os:cmd(Cmd),
  case file:read_file(Filename) of
    {ok, Data} ->
      File = list_to_binary(lists:concat([Prefix, utils:time_str(), ".jpg"])),
      telegram_bot:aapi("sendPhoto", #{ chat_id => ChatId, filename => File, data => Data });
    Error ->
      ?ERROR("~p : ~p", [Filename, Error]),
      telegram_bot:text(ChatId, <<"Произошла ошибка, снимок получить не удалось"/utf8>>)
  end.

