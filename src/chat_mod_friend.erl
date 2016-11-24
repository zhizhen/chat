-module(chat_mod_friend).

-export([add_friend/0]).

add_friend() ->
    lager:info("this add friend!~p~n", [{}]).
