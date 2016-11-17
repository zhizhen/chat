-module(chat_mod_sync).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").


-export([client_sync/2]).

client_sync(#mqtt_client{client_id   = _ClientId,
                         client_pid  = ClientPid,
                         username    = Username}, _Opts) ->
    %SyncRecords = chat_sync:fetch(ClientId, Username),
    lager:info("client sync:~p~n", [{Username}]),
    SyncRecords = [],
    Sort = fun(#mqtt_message{id = MsgId1}, #mqtt_message{id = MsgId2}) ->
                   MsgId1 =< MsgId2
           end,
    Task = fun() ->
            lager:info("client pid: ~p~n", [{ClientPid}]),
            SessPid = emqttd_client:session(ClientPid),
            SyncMsgs = lists:append([chat_backend:sync_messages(SyncKey, Offset) ||
                    #chat_sync{synckey = SyncKey, offset = Offset} <- SyncRecords]),
            lists:foreach(fun(Msg) -> SessPid ! {dispatch, Msg} end, lists:sort(Sort, SyncMsgs))
    end,
    emqttd_pooler:async_submit(Task);

client_sync(Client, Opts) ->
    lager:info("unhandled client sync:~p~n", [{Client, Opts}]),
    ok.

