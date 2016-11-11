-module(chat_mod_message).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, message_published/2, message_acked/3, unload/1]).

load(Opts) ->
    emqttd_broker:hook('message.publish', {?MODULE, slimchat_message_published},
                       {?MODULE, message_published, [Opts]}),
    emqttd_broker:hook('message.acked', {?MODULE, slimchat_message_acked},
                       {?MODULE, message_acked, [Opts]}).

message_published(Message, _Opts) ->
    lager:info("unhandled message published : ~p~n", [{Message, _Opts}]),
    Message.

message_acked(_ClientId, _Message, _Opts) ->
    lager:info("unhandled message acked : ~p~n", [{_ClientId, _Message, _Opts}]),
    ok.

unload(_Opts) ->
    emqttd_broker:unhook('message.publish', {?MODULE, slimchat_message_published}),
    emqttd_broker:unhook('message.acked', {?MODULE, slimchat_message_acked}).

