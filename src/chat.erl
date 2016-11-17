-module(chat).

-export([load/0, broker/0]).

broker() ->
    {ok, Addr} = application:get_env(chat, broker), Addr.

load() ->
    chat_backend:load(),
    chat_mod_message:load([]),
    chat_mod_presence:load([]),
    ok.
