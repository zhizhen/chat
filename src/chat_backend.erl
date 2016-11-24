-module(chat_backend).

-include("chat.hrl").

-export([load/0, unload/0]).

-export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

-export([store_message/1, ack_message/2, sync_messages/2]).

-export([to_list/1]).

-ifdef(use_specs).

-callback onload() -> ok | {error, any()}.

-callback onunload() -> ok | {error, any()}.

-callback store_message(mqtt_message()) -> ok | {error, any()}.

-callback ack_message(ClientId :: binary(), mqtt_message()) -> ok | {error, any()}.

-callback sync_messages(chat_synckey(), binary()) -> [mqtt_message()].

-callback find_offline_msg(To :: binary()) -> [mqtt_message()]}.

-callback find_contacts(Username:: binary()) -> [chat_contact()].

-callback find_rooms(Username:: binary()) -> [chat_room()].

-else.

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
        [{onload, 0}, {onunload, 0}, {store_message, 1}, {ack_message, 2},
         {find_offline_msg, 1}, {find_contacts, 1}, {find_rooms, 1}];

behaviour_info(_Other) ->
        undefined.

-endif.

%%%=============================================================================
%%% API
%%%=============================================================================

load() ->
    with_backend(onload, []).

find_contacts(Username) ->
    with_backend(find_contacts, [Username]).

find_rooms(Username) ->
    with_backend(find_rooms, [Username]).

find_offline_msg(Username) ->
    with_backend(find_offline_msg, [Username]).

store_message(Message) ->
    with_backend(store_message, [Message]).

ack_message(ClientId, Message) ->
    with_backend(ack_message, [ClientId, Message]).

sync_messages(SyncKey, Offset) ->
    with_backend(sync_messages, [SyncKey, Offset]).

unload() ->
    with_backend(onunload, []).

to_list(#chat_contact{username = Name,
                          nick = Nick,
                          group = Group,
                          presence = Presence,
                          show = Show,
                          status = Status}) ->
    [{id, Name}, {nick, Nick},
     {group, Group},
     {presence, Presence},
     {show, Show}, {status, Status}];

to_list(#chat_room{name = Name, nick = Nick}) ->
    [{id, Name}, {nick, Nick}, {avatar, <<"">>}].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

with_backend(Fun, Args) ->
    {Backend, _Env} = application:get_env(chat, backend, {mnesia, []}),
    apply(backend_mod(Backend), Fun, Args).

backend_mod(Backend) ->
    list_to_atom("chat_backend_" ++ atom_to_list(Backend)).


