-module(chat_sync_sup).

%% API
-export([start_link/0]).

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Schedulers = erlang:system_info(schedulers),
    gproc_pool:new(chat_sync, hash, [{size, Schedulers}]),
    Children = lists:map(
                 fun(I) ->
                   Name = {chat_sync, I},
                   gproc_pool:add_worker(chat_sync, Name, I),
                   {Name, {chat_sync, start_link, [I]},
                               permanent, 10000, worker, [chat_sync]}
                 end, lists:seq(1, Schedulers)),
    {ok, {{one_for_all, 10, 100}, Children}}.


