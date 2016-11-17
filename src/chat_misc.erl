-module(chat_misc).

-compile(export_all).

now_to_secs() ->
    {M, S, _} = erlang:now(),  
    M * 1000000 + S.

random(N1, N2) ->
    N1 + random:uniform(N2 - N1).
