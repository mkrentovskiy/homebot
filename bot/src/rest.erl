-module(rest).
-export([
          request/6,
          from_json/2,
          to_json/1
        ]).

-include("wa.hrl").

request(Method, Type, URL, Expect, InHeaders, Body) ->
  Headers = maps:merge(InHeaders, #{
      <<"Accept">> => get_access_type(Type) ++ ", */*;q=0.9",
      <<"Content-Type">> => get_content_type(Type)
    }),  
  {ok, {_Scheme, _, Host, Port, Path, QS}} = http_uri:parse(URL),
  {ok, ConnPid} = gun:open(Host, Port, #{ protocols => [http] }),
  {ok, _Protocol} = gun:await_up(ConnPid, ?GUN_TIMEOUT),
  StreamRef = case lists:any(fun(I) -> I =:= Method end, [post, put]) of  
    true -> 
      EncBody = encode_body(Type, Body),
      gun:Method(ConnPid, Path ++ QS, maps:to_list(Headers#{ <<"content-length">> => byte_size(EncBody) }), EncBody);
    false when length(QS) =:= 0 ->
      gun:Method(ConnPid, lists:concat([Path, "?", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), maps:to_list(Headers));
    false when length(QS) =/= 0 ->
      gun:Method(ConnPid, lists:concat([Path, QS, "&", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), maps:to_list(Headers));
    false -> 
      gun:Method(ConnPid, Path ++ QS, maps:to_list(Headers))
  end,
  Resp = case gun:await(ConnPid, StreamRef, ?GUN_TIMEOUT) of
    {response, fin, Status, RespHeaders} ->
      case lists:any(fun(I) -> I =:= Status end, Expect) of
        true ->
          {ok, Status, RespHeaders, #{}};
        false when Expect =:= [] ->
          {ok, Status, RespHeaders, #{}};
        false ->
          {error, Status, RespHeaders, #{}}               
      end;    
    {response, nofin, Status, RespHeadersL} ->
      RespHeaders = maps:from_list(keys_to_lower(RespHeadersL)),
      case gun:await_body(ConnPid, StreamRef, ?GUN_TIMEOUT) of
        {ok, RespBody} -> 
          case lists:any(fun(I) -> I =:= Status end, Expect) of
            true ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false when Expect =:= [] ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false ->
              {error, Status, RespHeaders, parse(RespHeaders, RespBody)}               
          end;
        Error ->
          {error, Error}
      end;
    Any -> Any
  end,
  gun:shutdown(ConnPid),
  Resp.

from_json(Msg, Default) ->
  try jsx:decode(Msg, [return_maps]) of
    {error, Error} -> 
      ?ERROR("Error ~p in decoding ~p", [Error, Msg]),
      Default;
    {error, Error, Str} -> 
      ?ERROR("Error ~p in decoding ~p", [Error, Str]),
       Default;
    Data -> Data
  catch Exc:Exp -> 
    ?ERROR("Exception ~p:~p in decoding of ~p", [Exc, Exp, Msg]),
    Default 
  end.

to_json(Msg) ->
  try
    case jsx:encode(Msg) of
      Str when is_binary(Str) -> Str;
      Err -> 
        ?ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), 
        jsx:encode([])
    end
  catch 
    Exc:Exp -> 
      ?ERROR("Exception ~p:~p in encoding of ~p\n~p", [Exc, Exp, Msg, erlang:get_stacktrace()]),
      <<"{}">>
  end.

keys_to_lower(L) -> [{string:lowercase(K), V} || {K, V} <- L].

get_access_type(html)   -> "text/html";
get_access_type(qs)     -> "application/x-www-form-urlencoded";
get_access_type(_)      -> "application/json".

get_content_type(video) -> "video/mp4"; % TODO: Only for tests with youtube
get_content_type({multipart, _})  -> "multipart/form-data; boundary=xxxxxxxxXXXXXXXX";
get_content_type(html)  -> "text/html";
get_content_type(qs)    -> "application/x-www-form-urlencoded";
get_content_type(_)     -> "application/json".

encode_body(qs, Body) -> 
  cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> 
  Body;
encode_body({multipart, FileName}, Body) -> 
  {ok, FileType, Mime} = get_mime_from_extension(filename:extension(FileName)),
  NewBody = <<<<"--xxxxxxxxXXXXXXXX\r\n"
    "Content-Disposition: form-data; name=\"">>/binary, FileType/binary, <<"\"; filename=\"">>/binary, FileName/binary,<<"\"\r\n"
    "Content-Type: ">>/binary, Mime/binary, <<"\r\n"
    "\r\n">>/binary,
    Body/binary,
    <<"\r\n"
    "--xxxxxxxxXXXXXXXX--">>/binary>>,
  NewBody;
encode_body({video, _FileName}, Body) -> 
  Body;
encode_body(_, Body) -> 
  to_json(Body).

parse(Headers, Body) ->
  Unzip = case maps:get(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> 
      zlib:gunzip(Body);
    _ -> 
      Body
  end,
  case maps:get(<<"content-type">>, Headers, undefined) of
    undefined -> 
      parse_body("application/json", Unzip);
    Type -> 
      CType = hd(string:tokens(binary_to_list(Type), ";")),
      parse_body(CType, Unzip)
  end.

parse_body("application/json", Body) -> 
  from_json(Body, #{}); 
parse_body("text/javascript", Body) -> 
  from_json(Body, #{}); 
parse_body("application/x-www-form-urlencoded", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body));
parse_body("text/plain", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> 
  Body.

get_mime_from_extension(Extension) ->
  case Extension of
    <<".png">> ->
      {ok, <<"photo">>, <<"image/png">>};
    <<".jpg">> ->
      {ok, <<"photo">>, <<"image/jpeg">>};
    <<".jpeg">> ->
      {ok, <<"photo">>, <<"image/jpeg">>};
    <<".mp4">> ->
      {ok, <<"video">>, <<"video/*">>};
    <<".mpg">> ->
      {ok, <<"video">>, <<"video/*">>};
    _ ->
      {error, undefined, <<"">>}
  end.