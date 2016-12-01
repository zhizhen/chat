-module(chat_mod_presence).

-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_online/3, client_offline/3, unload/1]).

load(Opts) ->
    emqttd:hook('client.connected', fun ?MODULE:client_online/3, [Opts]),
    emqttd:hook('client.disconnected', fun ?MODULE:client_offline/3, [Opts]).


%client_online(_, #mqtt_client{username = Username}, _Opts)
%    when ?EMPTY(Username) -> ok;

client_online(Qos, Mqttc = #mqtt_client{client_id  = ClientId,
                                      client_pid = ClientPid,
                                      username   = Username}, Opts) ->
    %% Subscribe
    lager:info("client online :~p~n", [{Qos, Username}]),
    erlang:put(mqttc, Mqttc),
    emqttd_client:subscribe(ClientPid, [{<<"/sys/broadcast">>, 0}]).

client_offline(_Reason, _ClientId, _Opts) ->
    ok.

unload(_Opts) ->
    emqttd:unhook('client.connected', fun ?MODULE:client_onlune/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:client_offline/3).

