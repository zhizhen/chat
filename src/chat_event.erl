-module(chat_event).

-define(CHAT_EVENT_TABLE, chat_event_table).

-export([load/0, listen/2, emit/2]).

load() ->
    ets:new(?CHAT_EVENT_TABLE, [named_table]),
    listen(101000, fun chat_mod_friend:add_friend/0),
    listen(100000, fun chat_mod_login:login/0),
    ok.

listen(Code, Fun) ->
    ets:insert(?CHAT_EVENT_TABLE, {Code, Fun}).

emit(Code, Data) ->
    Fun = get_code_fun(Code),
    apply(Fun, []).

get_code_fun(101000) ->
    fun chat_mod_friend:add_friend/0;
get_code_fun(100000) ->
    fun chat_mod_login:login/0;
get_code_fun(Code) ->
    case ets:lookup(?CHAT_EVENT_TABLE, Code) of
        [] ->
            lager:error("not found fun :~p~n", [Code]);
        [{Code, Fun}] ->
            Fun
    end.
