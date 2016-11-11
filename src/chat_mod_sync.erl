-module(chat_mod_sync).

-include("chat.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-behaviour(emqttd_gen_mod).

-export([load/1, client_sync/3, unload/1]).

-define(EMPTY(Field), ((Field =:= undefined) orelse (Field =:= <<>>))).

load(Opts) ->
    emqttd:hook('client.connected', fun ?MODULE:client_sync/3, [Opts]).

client_sync(Qos, Client, Opts) ->
    lager:info("unhandled client sync:~p~n", [{Qos, Client, Opts}]),
    ok.

unload(_Opts) ->
    ok.
