-module(chat_mod_friend).

-export([add_friend/1]).

add_friend(Data) ->
    lager:info("this add friend!~p~n", [{Data}]).
