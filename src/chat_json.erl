-module(chat_json).

-export([encode/1, decode/1]).

encode(Data) ->
    iolist_to_binary(mochijson2:encode(Data)).

decode(Data) ->
    Decode = mochijson2:decoder([{format, proplist}]),
    {ok, Decode(Data)}.
