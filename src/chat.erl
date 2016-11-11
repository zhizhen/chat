-module(chat).

-export([load/0]).

load() ->
    %chat_backend:load(),
    chat_mod_sync:load([]),
    %chat_mod_message:load([]),
   % chat_mod_presence:load([]),
    ok.
