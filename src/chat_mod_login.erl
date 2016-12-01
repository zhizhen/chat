-module(chat_mod_login).
-include("chat.hrl").
-export([login/1]).

login(Data) ->
    %% 验证用户信息
    
    Uuid  = g_v(<<"uuid">>, Data),
    Dinfo = g_v(<<"dinfo">>, Data),
    Pinfo = g_v(<<"pinfo">>, Data),
    Token = g_v(<<"token">>, Data),
    Uinfo = g_v(<<"uinfo">>, Data),

    {Ret, Msg} = check_login(Data),

    Friends = chat_backend_mysql:find_contacts(Uuid),
    UserData = #{uuid => Uuid, username => Uuid, friends => Friends},
    erlang:put(user_data, UserData),
    
    chat_mod_channel:load(),

    Now = chat_misc:now_to_secs(),
    RetData = [[{code, 100001},
	       {data, [{ret, Ret},
		       {msg, unicode:characters_to_binary("登陆成功")},
		       {token, Token},
		       {servertime, Now}
		      ]
	       }]],

    Json = mochijson2:encode(RetData),

    Payload = list_to_binary(Json),
    
    lager:info("this login ! ~p~n", [{Uuid, Dinfo, Pinfo, Token, Uinfo, Friends, Json, Payload}]),

    emqttd:publish(
      emqttd_message:make(Uuid, qos0, <<"/sys/", Uuid/binary, "/r">>, Payload)
     ),
    ok.

check_login(Data) ->
    
    {0, ""}.

g_v(Key, List) ->
    maps:get(Key, List).
