-module(chat_event).

-define(CHAT_EVENT_TABLE, chat_event_table).

-export([load/0, listen/2, emit/2]).

load() ->
    ets:new(?CHAT_EVENT_TABLE, [named_table]),
    listen(101000, fun chat_mod_friend:add_friend/1),
    listen(100000, fun chat_mod_login:login/1),
    listen(104001, fun chat_mod_channel:join/1),
    ok.

listen(Code, Fun) ->
    ets:insert(?CHAT_EVENT_TABLE, {Code, Fun}).

emit(Code, Data) ->
    case get_code_fun(Code) of
	{error, Reason} ->
	    lager:error("get fun error : ~p~n", [{Code, Reason}]);
	Fun ->
	    apply(Fun, [Data])
    end.

get_code_fun(101000) ->
    fun chat_mod_friend:add_friend/1;
get_code_fun(100000) ->
    fun chat_mod_login:login/1;
get_code_fun(Code) ->
    case ets:lookup(?CHAT_EVENT_TABLE, Code) of
        [] ->
	    {error, not_found};
        [{Code, Fun}] ->
            Fun
    end.
