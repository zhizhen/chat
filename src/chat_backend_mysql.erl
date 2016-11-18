-module(chat_backend_mysql).

-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-behaviour(chat_backend).

%% chat_backend callbacks
-export([onload/0, onunload/0]).

-export([store_message/1, ack_message/2]).

-export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

onload() ->
    {ok, {mysql, Env}} = application:get_env(chat, backend),
    lager:info("mysql env : ~p~n", [Env]),
    ok.

find_contacts(Username) ->
    [].

find_rooms(Username) ->
    [].

find_offline_msg(Endpoint) ->
    [].

store_message(#mqtt_message{payload = Payload}) ->
    ok.

ack_message(ClientId, Message) ->
    ok.

onunload() ->
    ok.
