-module(chat_sync).

-include("chat.hrl").

%% Mnesia Callbacks
-export([mnesia/1]).

-boot_mnesia({mnesia, [boot]}).
-copy_mnesia({mnesia, [copy]}).

%% API Function Exports
-export([start_link/1, store/2, fetch/2]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pool, id}).

mnesia(boot) ->
    ok = emqttd_mnesia:create_table(chat_sync, [
                {type, ordered_set},
                {ram_copies, [node()]},
                {record_name, slimchat_sync},
                {attributes, record_info(fields, slimchat_sync)}]);

mnesia(copy) ->
    ok = emqttd_mnesia:copy_table(chat_sync).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(I) ->
    gen_server:start_link(?MODULE, [I], []).

store(SyncKey = #slimchat_synckey{client = ClientId}, Offset) ->
    Pid = gproc_pool:pick_worker(?MODULE, ClientId),
    gen_server:cast(Pid, {store, SyncKey, Offset}).

fetch(ClientId, Username) ->
    SyncKey = #slimchat_synckey{client = ClientId, username = Username, _ = '_'},
    mnesia:dirty_match_object(#slimchat_sync{synckey = SyncKey, _ = '_'}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Id]) ->
    gproc_pool:connect_worker(?MODULE, {?MODULE, Id}),
    {ok, #state{id = Id}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({store, SyncKey, Offset}, State) ->
    mnesia:dirty_write(#slimchat_sync{synckey = SyncKey, offset = Offset}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{id = Id}) ->
    gproc_pool:disconnect_worker(?MODULE, {?MODULE, Id}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

