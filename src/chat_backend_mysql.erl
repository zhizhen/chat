-module(chat_backend_mysql).

-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-behaviour(chat_backend).

%% chat_backend callbacks
-export([onload/0, onunload/0]).

-export([store_message/1, ack_message/2]).

-export([find_contacts/1, find_channels/1, find_rooms/1, find_offline_msg/1]).

onload() ->
    {ok, PoolArgs} = application:get_env(chat, mysql_pool),
    {ok, {mysql, MysqlArgs}} = application:get_env(chat, backend),
    PoolArgs1 = [{strategy, fifo}, {name, {local, mysql_pool}}, {worker_module, mysql}| PoolArgs],
    PoolSpec = poolboy:child_spec(mysql_pool, PoolArgs1, MysqlArgs),
    supervisor:start_child(chat_sup, PoolSpec),
    lager:info("mysql pool start success !: ~p~n", [{PoolSpec}]),
    ok.

find_contacts(Uid) ->
    Table = "oc_user_relation_demo",
    TableUser = "oc_user_demo",
    StatementRef = "select a.uid,a.friend_uid,a.remark,b.username 
    as friend_name from " ++ Table ++ " as a LEFT JOIN " ++ TableUser ++ 
    " as b on a.friend_uid=b.uid where a.uid=? and a.status=1",
    Params = [Uid],
    {ok, _Fields, Contacts} = poolboy:transaction(mysql_pool, fun(MysqlConn) ->
                mysql:query(MysqlConn, StatementRef, Params)
        end),
    Contacts.

find_channels(Uid) ->
    Table = "oc_channel_user_list_demo",
    StatementRef = "select tid from " ++ Table ++ " WHERE uid=?",
    Params = [Uid],
    {ok, _Fields, Channels} = 
	poolboy:transaction(mysql_pool, 
			    fun(MysqlConn) ->
				    mysql:query(MysqlConn, StatementRef, Params)
			    end),
    Channels.

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


