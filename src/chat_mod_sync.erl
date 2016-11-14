-module(chat_mod_sync).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_sync/3, unload/1]).

-define(EMPTY(Field), ((Field =:= undefined) orelse (Field =:= <<>>))).

load(Opts) ->
    emqttd:hook('client.connected', fun ?MODULE:client_sync/3, [Opts]).

client_sync(0, #mqtt_client{username = Username}, _Opts)
    when ?EMPTY(Username) -> ok;

client_sync(0, #mqtt_client{client_id   = ClientId,
                            client_pid  = ClientPid,
                            username    = Username}, _Opts) ->
    SyncRecords = chat_sync:fetch(ClientId, Username),
    Sort = fun(#mqtt_message{id = MsgId1}, #mqtt_message{id = MsgId2}) ->
                   MsgId1 =< MsgId2
           end,
    Task = fun() ->
                   SessPid = emqttd_client:session(ClientPid),
                   SyncMsgs = lists:qppend([chat_backend:sync_messages(SyncKey, Offset) ||
                                            #slimchat_sync{synckey = SyncKey, offset = Offset} <- SyncRecords]),
                   lists:foreach(fun(Msg) -> SessPid ! {dispatch, Msg} end, lists:sort(Sort, SyncMsgs))
           end,
    emqttd_pooler:async_submit(Task);

client_sync(Qos, Client, Opts) ->
    lager:info("unhandled client sync:~p~n", [{Qos, Client, Opts}]),
    ok.

unload(_Opts) ->
    ok.
