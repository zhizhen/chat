-module(chat_mod_presence).

-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_online/3, client_offline/3, unload/1]).

load(Opts) ->
    emqttd:hook('client.connected', fun ?MODULE:client_online/3, [Opts]),
    emqttd:hook('client.disconnected', fun ?MODULE:client_offline/3, [Opts]).


client_online(0, #mqtt_client{username = Username}, _Opts)
    when ?EMPTY(Username) -> ok;

client_online(0, Mqttc = #mqtt_client{client_id  = _ClientId,
                                      client_pid = ClientPid,
                                      username   = Username}, Opts) ->
    %% Subscribe
    lager:info("client online :~p~n", [{Username}]),
    R = 
    [begin Topics = [{topic_r(chat, U), 1}, {topic_w(chat, U), 1}],
           emqttd_client:subscribe(ClientPid, Topics),
           Topics
    end || U <- get_friend_list(Username)],
    lager:info("topisc : ~p~n", [{R}]),
    chat_mod_sync:client_sync(Mqttc, Opts),
    ok.

client_offline(_Reason, _ClientId, _Opts) ->
    ok.

unload(_Opts) ->
    emqttd:unhook('client.connected', fun ?MODULE:client_onlune/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:client_offline/3).

topic_r(chat, Username) -> <<"/sys/", Username/binary, "/r">>.
topic_w(chat, Username) -> <<"/sys/", Username/binary, "/w">>.

get_friend_list(<<"123">>) -> [<<"123">>, <<"456">>];
get_friend_list(<<"456">>) -> [<<"456">>, <<"123">>];
get_friend_list(_) -> [].
