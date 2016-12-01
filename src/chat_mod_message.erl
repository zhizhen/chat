-module(chat_mod_message).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, message_published/2, message_acked/4, unload/1]).

load(Opts) ->
    emqttd:hook('message.publish', fun ?MODULE:message_published/2, [Opts]),
    emqttd:hook('message.acked', fun ?MODULE:message_acked/4, [Opts]).

message_published(Message = #mqtt_message{
			       id = MsgId, from = {ClientId, Username},
			       topic = Topic, payload = PayLoad}, _Opts) 
  when Topic =:= <<"/sys/", ClientId/binary, "/w">> ->
    {ok, Maps} = chat_json:decode(binary_to_list(PayLoad)),
%%    lager:info("chat message : ~p~n", [{Maps}]),
    Fun = fun(Msg) ->
            Code = maps:get(<<"code">>, Msg),
            Data = maps:get(<<"data">>, Msg),
            %% 分发到不同模块
            chat_event:emit(Code, Data)
    end,
    lists:foreach(Fun, Maps),
    {ok, Message};

message_published(Message, _Opts) ->
    %lager:info("unhandled message published : ~p~n", [{Message, _Opts}]),
    {ok, Message}.

message_acked(_ClientId, _Username, _Message, _Opts) ->
    lager:info("unhandled message acked : ~p~n", [{_ClientId, _Username, _Message, _Opts}]),
    ok.

unload(_Opts) ->
    emqttd:unhook('message.publish', fun ?MODULE:message_published/2),
    emqttd:unhook('message.acked', fun ?MODULE:message_acked/4).

