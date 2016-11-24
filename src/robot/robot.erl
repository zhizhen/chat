-module(robot).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, stop/0]).
-export([add_friend/2, publish/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mqttc, client, friend}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Uid, FriendUid) ->
    Uidstr = list_to_atom("robot_" ++ integer_to_list(Uid)),
    gen_server:start_link({local, Uidstr}, ?MODULE, [Uid, FriendUid], []).

stop() ->
    gen_server:call(?SERVER, stop).

add_friend(Pid, Uid) ->
    gen_server:call(Pid, {add_friend, Uid}).

publish(Uid, Msg) when is_integer(Uid) ->
    Uidstr = list_to_atom("robot_" ++ integer_to_list(Uid)),
    publish(whereis(Uidstr), Msg);
publish(Pid, Msg) when is_pid(Pid) ->
    gen_server:call(Pid, {publish, Msg}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Uuid, Friend]) ->
    Uidstr = erlang:integer_to_binary(Uuid),
    Username = list_to_binary("robot_" ++ integer_to_list(Uuid)),
    {ok, C} = emqttc:start_link([{host, "localhost"}, 
                                 {client_id, Uidstr},
                                 {reconnect, 0},
                                 {username, Username},
                                 {logger, {console, info}}]),
    Data= [[{code, 100000},
           {data, [{uid, Uuid},
                   {name, ""},
                   {passwd, ""}]}]],

    Json = mochijson2:encode(Data),
    Payload = list_to_binary(Json),
    emqttc:publish(C, <<"/sys/", Uidstr/binary, "/w">>, Payload, [{qos, 1}]),
    %erlang:send_after(3000, self(), chat_to_friend),
    {ok, #state{mqttc = C, client = Uidstr, friend = Friend}}.

handle_call({add_friend, FriendUid}, _From, State = #state{mqttc   = C,
                                                           client   = Uidstr}) ->
    Data= [[{code, 101000},
            {data, [{uid, FriendUid},
                    {reason, ""}]}]],
    Json = mochijson2:encode(Data),
    Payload = list_to_binary(Json),
    emqttc:publish(C, <<"/sys/", Uidstr/binary, "/w">>, Payload, [{qos, 1}]),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({publish, Msg}, _From, State = #state{mqttc  = C,
                                                 client = Uidstr}) ->
    Payload = list_to_binary(Msg),
    emqttc:publish(C, <<"/sys/", Uidstr/binary, "/w">>, Payload, [{qos, 1}]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(chat_to_friend, State = #state{mqttc    = C, 
                                           client   = Uid, 
                                           friend = Friend}) ->
    Username = list_to_atom("robot_" ++ binary_to_list(Uid)),
    FriendUid= erlang:integer_to_binary(Friend),
    Msg = "hellooooooo",
    Msgid = 1,
    Data = [[
            {code, 102000},
            {data, [
                    {ret, 0},
                    {msg, ""},
                    {fromuid, Uid},
                    {fromname, Username},
                    {touid, FriendUid},
                    {msgtype, 1},
                    {msgdata, [
                            {direct, 0},
                            {fromId, Uid},
                            {body, [
                                    {type, 1},
                                    {text, Msg}
                                ]},
                            {toId, FriendUid},
                            {status, 1},
                            {type, 1},
                            {convsId, "s_123"},
                            {time, chat_misc:now_to_secs()},
                            {isAcked, false},
                            {msgId, Msgid},
                            {fromName, Username},
                            {isDelivered, false},
                            {convsType, 1},
                            {isRead, true}
                        ]},
                    {msgid, Msgid}
                ]}
        ]],
    Json = mochijson2:encode(Data),
    Payload = list_to_binary(Json),
    emqttc:publish(C, <<"/sys/", FriendUid/binary, "/w">>, Payload, [{qos, 1}]),
    erlang:send_after(3000, self(), chat_to_friend),
    {noreply, State};

%% Receive Messages
handle_info({publish, Topic, Payload}, State) ->
    io:format("Message from ~s: ~p~n", [Topic, Payload]),
    {noreply, State};

%% Client connected
handle_info({mqttc, C, connected}, State = #state{mqttc = C}) ->
    io:format("Client ~p is connected~n", [C]),
    {noreply, State};

%% Client disconnected
handle_info({mqttc, C,  disconnected}, State = #state{mqttc = C}) ->
    io:format("Client ~p is disconnected~n", [C]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

