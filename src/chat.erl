-module(chat).
-include("chat.hrl").

-export([load/0, broker/0]).

broker() ->
    {ok, Addr} = application:get_env(chat, broker), Addr.

load() ->
    create_ets(),
    chat_event:load(),
    chat_backend:load(),
    chat_mod_message:load([]),
    chat_mod_presence:load([]),
    ok.

create_ets() ->
    ets:new(channel_data, [named_table, public, {keypos, #chat_channel.id}]),
    ok.
