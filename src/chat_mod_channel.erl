-module(chat_mod_channel).
-include("chat.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-export([load/0, join/1]).

load() ->
    lager:info("load channel list : ~p~n", [{}]),
    UserData = erlang:get(user_data),
    #{uuid := Uuid} = UserData,

    case maps:is_key(channel_list, UserData) of
	true ->
	    #{channel_list := ChannelList} = UserData,
	    lists:foreach(fun(Tid) -> subscribe_channel(Uuid, Tid) end, ChannelList);
	false ->
	    Channels = chat_backend_mysql:find_channels(Uuid),
	    lager:info("load channels : ~p~n", [{Channels}]),
	    UserData1 = UserData#{channel_list => Channels},
	    erlang:put(user_data, UserData1)
    end.

subscribe_channel(Uuid, Tid) ->
    ok.

join(Data) ->
    #{
      <<"channelid">> := ChannelId,
      <<"channelname">> := ChannelName,
      <<"channeltype">> := ChannelType,
      <<"savetype">> := SaveType
     } = Data,
    lager:info("join channel : ~p~n", [{Data}]),
    UserData = erlang:get(user_data),
    #{uuid := Uuid, username := UserName} = UserData,

    %% 缓存频道信息
    ChannelData = 
	case ets:lookup(channel_data, ChannelId) of
	    [] ->
		CD = #chat_channel{id = ChannelId, name = ChannelName,
				  type = ChannelType, savetype = SaveType,
				  admin_uid = Uuid, index = 0, uids = []},
		ets:insert(channel_data, CD),
		CD;
	    [C] ->
		C
	end,

    AdminId = ChannelData#chat_channel.admin_uid,

    #mqtt_client{client_pid = ClientPid} = erlang:get(mqttc),
    emqttd_client:subscribe(ClientPid, [{<<"/chat/channel/", ChannelId/binary>>, 0}]),
    
    RetData = [[{code, 104004},
	       {data, [{ret, 0},
		       {id, null},
		       {msg, unicode:characters_to_binary( "加入频道")},
		       {fromuid, Uuid},
		       {ownerid, AdminId},
		       {channelid, ChannelId},
		       {channelname, ChannelName},
		       {channeltype, ChannelType},
		       {savetype, SaveType},
		       {uids, [{uid, Uuid}, {name, UserName}]}
                      ]
	       }]],

    Json = mochijson2:encode(RetData),
    Payload = list_to_binary(Json),

    emqttd:publish(
      emqttd_message:make(Uuid, qos0, <<"/sys/", Uuid/binary, "/r">>, Payload)
     ),
    ok.
