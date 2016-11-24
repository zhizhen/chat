-module(chat_backend_mysql).

-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-behaviour(chat_backend).

%% chat_backend callbacks
-export([onload/0, onunload/0]).

-export([store_message/1, ack_message/2]).

-export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

onload() ->
    {ok, PoolArgs} = application:get_env(chat, mysql_pool),
    {ok, {mysql, MysqlArgs}} = application:get_env(chat, backend),
    poolboy:child_spec(PoolName, PoolArgs, MysqlArgs),
    lager:info("mysql pool start success !: ~p~n", [{PoolArgs, MysqlArgs}]),
    ok.

find_contacts(Username) ->
    StatementRef = "SHOW TABLES;",
    Params = [],
    poolboy:transaction(mysql_pool, fun(MysqlConn) ->
                mysql:execute(MysqlConn, StatementRef, Params)
        end).

find_rooms(Username) ->
    [].

find_offline_msg(Endpoint) ->
    [].

store_message(#mqtt_message{payload = Payload}) ->
    ok.

ack_message(ClientId, Message) ->
    ok.

onunload() ->
    ok.
