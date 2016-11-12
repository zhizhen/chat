-module(chat_mod_message).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, message_published/2, message_acked/3, unload/1]).

load(Opts) ->
    emqttd:hook('message.publish', fun ?MODULE:message_published/2, [Opts]),
    emqttd:hook('message.acked', fun ?MODULE:message_acked/3, [Opts]).

message_published(Message = #mqtt_message{id = MsgId, qos = 1,
                                          from = ClientId,
                                          topic = Topic = <<"chat/user/", _To/binary>>},
                  _Opts) ->
    lager:info("chat message : ~p~n", [{Message}]),
    case emqttd_cm:lookup(ClientId) of
        #mqtt_client{username = Username} ->
            SyncKey = #slimchat_synckey{client = ClientId, username = Username, pubsub = publish, topic = Topic},
            chat_sync:store(SyncKey, MsgId);
        undefined ->
            lager:error("cannot find client: ~s", [ClientId])
    end,
    {ok, Message};

message_published(Message, _Opts) ->
    %lager:info("unhandled message published : ~p~n", [{Message, _Opts}]),
    {ok, Message}.

message_acked(_ClientId, _Message, _Opts) ->
    lager:info("unhandled message acked : ~p~n", [{_ClientId, _Message, _Opts}]),
    ok.

unload(_Opts) ->
    emqttd:unhook('message.publish', fun ?MODULE:message_published/2),
    emqttd:unhook('message.acked', fun ?MODULE:message_acked/3).

