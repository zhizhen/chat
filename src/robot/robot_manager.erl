-module(robot_manager).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-record(robot, {
        uid,
        pid,
        friend
    }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0]).
-export([login_many/1]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

login_many(N) ->
    Pid = whereis(?MODULE),
    gen_server:call(Pid, {login_many, N}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #{uid => 1, robots => []}}.

handle_call({login_many, N}, _From, #{uid := Uid, robots := Robots} = State) ->
    Robots1 = 
    [begin
                FriendUid = chat_misc:random(Uid, Uid + N),
                io:format("start robot : ~p~n", [{X, FriendUid}]),
                {ok, Pid} = robot:start_link(X, FriendUid),
                gen_server:call(Pid, {add_friend, FriendUid}),
                Robot = #robot{uid = X, friend = FriendUid, pid = Pid},
                Robot
        end|| X <- lists:seq(Uid, Uid + N)],
    {reply, Robots1, State#{uid => Uid + N, robots => Robots ++ Robots1}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    io:format("unhandled call : ~p~n", [{_Request, State}]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

