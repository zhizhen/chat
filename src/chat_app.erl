-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %application:ensure_all_started(emqttd),
    {ok, Sup} = chat_sup:start_link(),
    chat:load(),
    {ok, Sup}.

stop(_State) ->
    ok.
