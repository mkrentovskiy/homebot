-module(telegram_bot).
-behaviour(gen_server).
-export([
          text/2,
          api/2,
          aapi/2,
          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").
 
-define(API_HOST, "api.telegram.org").
-define(API_PORT, 443).
-define(UPDATE_REPEAT, 3).

text(undefined, _) ->
  ok;
text(ChatId, Text) ->
  api("sendMessage", #{ chat_id => ChatId, text => Text }).

api(Method, Params) ->
  gen_server:call(?MODULE, {api, Method, Params}).

aapi(Method, Params) ->
  gen_server:cast(?MODULE, {api, Method, Params}).

start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) ->
  ?AFTER(0, update),
  {ok, connect(Params#{ conn_pid => undefined, update_stream => undefined, update_from => undefined, data => <<>> })}.
%
% gen_server
%

handle_call({api, Method, Params}, _From, State) ->  
  api_call(Method, Params, State);
handle_call(_Msg, _From, State) ->  
  {reply, ok, State}.

handle_cast({api, Method, Params}, State) ->  
  {reply, Res, NewState} = api_call(Method, Params, State),
  % ?INFO("Call async ~p : ~p - ~p", [Method, Params, Res]),
  {noreply, NewState};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(update, State) ->
  {noreply, updates(State)};
handle_info({gun_response, _ConnPid, _StreamRef, _Type, _Status, _Headers}, State) ->
  {noreply, State};
handle_info({gun_data, _ConnPid, StreamRef, nofin, Data} , #{ data := D, update_stream := StreamRef } = State) ->
  {noreply, State#{ data := <<D/binary, Data/binary>> }};
handle_info({gun_data, _ConnPid, StreamRef, fin, Data} , #{ data := D, update_stream := StreamRef } = State) ->
  ?AFTER(0, update),
  {noreply, process(<<D/binary, Data/binary>>, State#{ data := <<>> })};
handle_info({gun_data, ConnPid, _StreamRef, _, _}, #{ conn_pid := UConnPid } = State) when UConnPid =/= ConnPid ->
  gun:shutdown(ConnPid),
  {noreply, State};
handle_info({gun_up, ConnPid, _}, #{ conn_pid := ConnPid } = State) ->
  {noreply, updates(State)};
handle_info({gun_down, _, _, _, _, _}, State) ->
  {noreply, State};
handle_info(Info, State) ->
  ?INFO("GOT INFO : ~p", [Info]), 
  {noreply, State}.

terminate(_Reason, #{ conn_pid := ConnPid }) when is_pid(ConnPid) ->
  gun:close(ConnPid), 
  ok;
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% local
%

connect(State) ->
  {ok, ConnPid} = gun:open(?API_HOST, ?API_PORT),
  {ok, _Protocol} = gun:await_up(ConnPid),
  State#{ conn_pid := ConnPid }.

updates(#{ conn_pid := ConnPid, update_from := From, token := Token } = State) ->
  URL = lists:concat(case From of
      undefined -> ["/bot", Token, "/getUpdates?limit=1&timeout=3600"];
      _ -> ["/bot", Token, "/getUpdates?offset=", From, "&limit=1&timeout=3600"]
    end),
  StreamRef = gun:get(ConnPid, URL),
  State#{ update_stream := StreamRef }.

api_call(Method, #{ chat_id := ChatId, filename := Filename, data := FileData } = Params, #{ token := Token } = State) ->
  URL = lists:concat(["https://", ?API_HOST, "/bot", 
    Token, "/", Method, 
    "?chat_id=", ChatId, 
    "&disable_notification=", maps:get(disable_notification, Params, "true"),
    "&caption=", maps:get(caption, Params, "")]),
  Res = case rest:request(post, {multipart, Filename}, URL, [200], #{}, FileData) of
    {ok, _, _, Reply} -> pass(Reply);
    Error -> {error, Error}
  end,
  {reply, Res, State};
api_call(Method, Params, #{ token := Token } = State) ->
  URL = lists:concat(["https://", ?API_HOST, "/bot", Token, "/", Method]),
  Res = case rest:request(post, json, URL, [200], #{}, Params) of
    {ok, _, _, Reply} -> pass(Reply);
    Error -> {error, Error}
  end,
  {reply, Res, State}.

pass(#{ <<"ok">> := true, <<"result">> := Result }) -> 
  {ok, Result};
pass(Error) -> 
  {error, Error}.

process(Msg, State) when is_binary(Msg) ->
  process(rest:from_json(Msg, #{}), State);
process(Msg, #{ update_from := FirstId } = State) ->
  case pass(Msg) of
    {ok, Events} -> 
      LastId = lists:foldl(fun(E, Id) -> process_each(E, Id, State) end, case FirstId of undefined -> 0; _ -> FirstId end, Events),
      State#{ update_from := LastId + 1 };
    Error -> 
      ?ERROR("Can't process events - ~p", [Error]),
      State
  end.

process_each(#{ <<"update_id">> := Id, <<"message">> := Message }, _, #{ trusted_list := Trust } = State) ->
  From = maps:get(<<"from">>, Message, #{}),
  case lists:any(fun({Key, Value}) -> Value =:= maps:get(Key, From, undefined) end, Trust) of
    true ->
      msg(Message, State);
    false ->
      ?ERROR("Unauth user ~p for ~p", [From, Message])
  end,
  Id;
process_each(_, Id, _) -> Id.

msg(#{ <<"chat">> := #{ <<"id">> := ChatId }, <<"text">> := Text }, State) ->
  text(Text, ChatId, State);
msg(Msg, _) ->
  ?INFO("MSG ~p", [Msg]).


text(<<"Камера"/utf8>>, ChatId, _State) ->
  cam(ChatId);
text(<<"Свет"/utf8>>, ChatId, State) ->
  send(ChatId, <<"Какой свет сделать?"/utf8>>, [
      [#{ text => <<"Включить"/utf8>> }, #{ text => <<"Выключить"/utf8>> }], 
      [#{ text => <<"Желтый"/utf8>> }],
      [#{ text => <<"Зеленый"/utf8>> }],
      [#{ text => <<"Голубой"/utf8>> }],
      [#{ text => <<"Розовый"/utf8>> }]
    ], State);
text(<<"Включить"/utf8>>, ChatId, State) ->
  light(ChatId, "fff", State);
text(<<"Выключить"/utf8>>, ChatId, State) ->
  light(ChatId, "000", State);
text(<<"Желтый"/utf8>>, ChatId, State) ->
  light(ChatId, "ff0", State);
text(<<"Зеленый"/utf8>>, ChatId, State) ->
  light(ChatId, "0f0", State);
text(<<"Голубой"/utf8>>, ChatId, State) ->
  light(ChatId, "0ff", State);
text(<<"Розовый"/utf8>>, ChatId, State) ->
  light(ChatId, "f0f", State);
text(<<"/cam">>, ChatId, _State) ->
  cam(ChatId);
text(<<"/lon">>, ChatId, State) ->
  light(ChatId, "fff", State);
text(<<"/loff">>, ChatId, State) ->
  light(ChatId, "000", State);
text(<<"/l ", Rest/binary>>, ChatId, State) ->
  light(ChatId, binary_to_list(Rest), State);
text(_, ChatId, State) ->
  send(ChatId, <<"Команда не распознана. Допустимые типы:
    /cam  - получить фото с внешней камеры
    /lon  - включить свет
    /loff - выключить свет
    /l [RGB] - выставить цвет
    "/utf8>>, [[#{ text => <<"Камера"/utf8>> }], [#{ text => <<"Свет"/utf8>> }]], State).

send(ChatId, Text, State) ->
  api_call("sendMessage", #{ chat_id => ChatId, text => Text }, State).
send(ChatId, Text, Keyboard, State) ->
  api_call("sendMessage", #{ chat_id => ChatId, text => Text, reply_markup => #{
      keyboard => Keyboard,
      one_time_keyboard => true,
      selective => true
    }}, State).

cam(ChatId) ->
  operations:cam(ChatId).

light(ChatId, RGB, State) when length(RGB) =:= 3 ->
  FullRGB = lists:foldl(fun(I, A) -> A ++ [I, I] end, "", RGB),
  light(ChatId, FullRGB, State);
light(ChatId, RGB, State) when length(RGB) =:= 6 ->
  RGB1 = string:uppercase(RGB),
  case lists:all(fun(I) -> (I >= $0) and (I =< $F) end, RGB1) of
    true ->
      operations:light(ChatId, RGB1);
    false -> 
      send(ChatId, <<"Неверная кодировка цвета"/utf8>>, State)
  end;
light(ChatId, _RGB, State) ->
  send(ChatId, <<"Неверный формат цвета"/utf8>>, State).
