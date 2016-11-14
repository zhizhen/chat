-module(robot).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).
-export([publish/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mqttc, client}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Uuid) ->
    Uidstr = list_to_atom("robot_" ++ integer_to_list(Uuid)),
    gen_server:start_link({local, Uidstr}, ?MODULE, [Uuid], []).

stop() ->
    gen_server:call(?SERVER, stop).

publish(Uuid, Msg) ->
    Uidstr = list_to_atom("robot_" ++ integer_to_list(Uuid)),
    gen_server:call(whereis(Uidstr), {publish, Msg}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Uuid]) ->
    Uidstr = erlang:integer_to_binary(Uuid),
    {ok, C} = emqttc:start_link([{host, "localhost"}, 
                                 {client_id, Uidstr},
                                 {reconnect, 0},
                                 {username, Uidstr},
                                 {logger, {console, info}}]),
    {ok, #state{mqttc = C, client = Uidstr}}.

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

%% Receive Messages
handle_info({publish, Topic, Payload}, State) ->
    io:format("Message from ~s: ~p~n", [Topic, Payload]),
    {noreply, State};

%% Client connected
handle_info({mqttc, C, connected}, State = #state{mqttc = C, client = Uidstr}) ->
    io:format("Client ~p is connected~n", [C]),
%    emqttc:subscribe(C, <<"/sys/", Uidstr/binary, "/r">>, 1),
%    emqttc:subscribe(C, <<"/sys/", Uidstr/binary, "/w">>, 1),

%    [begin emqttc:subscribe(Clientid, <<"/sys/", Clientid/binary, "/r">>, 1),
%           emqttc:subscribe(Clientid, <<"/sys/", Clientid/binary, "/w">>, 1)
%    end|| Clientid <- get_friend_list(Uidstr)],
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

