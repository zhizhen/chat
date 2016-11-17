-module(chat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = chat_sup:start_link(),
    chat:load(),
    start_listeners(),
    {ok, Sup}.

start_listeners() ->
    {ok, Listeners} = application:get_env(listeners),
    [start_listener(Listener) || Listener <- Listeners].

start_listener({http, Port, SockOpts}) ->
    MFArgs = {chat_http, handle_request, []},
    mochiweb:start_http(Port, SockOpts, MFArgs).

stop(_State) ->
    ok.
