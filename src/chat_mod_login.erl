-module(chat_mod_login).

-export([login/0]).

login() ->
    lager:info("this login ! ~p~n", [{}]).
