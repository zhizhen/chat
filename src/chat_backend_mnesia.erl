-module(chat_backend_mnesia).

%% -include("chat.hrl").
%% -include_lib("emqttd/include/emqttd.hrl").

%% -behaviour(chat_backend).

%% %% chat_backend callbacks
%% -export([onload/0, onunload/0]).

%% -export([store_message/1, ack_message/2, sync_messages/2]).

%% -export([find_contacts/1, find_rooms/1, find_offline_msg/1]).

%% -export([add_contact/1, del_contact/1, add_room/1, del_room/1]).


%% onload() ->
%%     ets:new(chat_message, [ordered_set, named_table, public]),
%%     mnesia:create_table(chat_contact, [
%%             {type, ordered_set},
%%             {ram_copies, [node()]},
%%             {record_name, chat_contact},
%%             {attributes, record_info(fields, chat_contact)}
%%         ]),
%%     mnesia:add_table_copy(chat_contact, node(), ram_copies),

%%     mnesia:create_table(chat_roster, [
%%             {type, bag},
%%             {ram_copies, [node()]},
%%             {record_name, chat_roster},
%%             {attributes, record_info(fields, chat_roster)}
%%         ]),
%%     mnesia:add_table_copy(chat_roster, node(), ram_copies),

%%     mnesia:create_table(chat_room, [
%%             {type, ordered_set},
%%             {ram_copies, [node()]},
%%             {record_name, chat_room},
%%             {attributes, record_info(fields, chat_room)}
%%         ]),
%%     mnesia:add_table_copy(chat_room, node(), ram_copies),

%%     mnesia:create_table(chat_member, [
%%             {type, bag},
%%             {ram_copies, [node()]},
%%             {record_name, chat_member},
%%             {attributes, record_info(fields, chat_member)}
%%         ]),
%%     mnesia:add_table_copy(chat_member, node(), ram_copies).

%% find_contacts(Username) ->
%%     CNames = [CName || #chat_roster{cname = CName}
%%         <- mnesia:dirty_read(chat_roster, Username)],
%%     lists:append([mnesia:dirty_read(chat_contact, CName) || CName <- CNames]).

%% add_contact(Contact) when is_record(Contact, chat_contact) ->
%%     mnesia:transaction(fun mnesia:write/1, [Contact]).

%% del_contact(Name) ->
%%     mnesia:transaction(fun mnesia:delete/1, [{chat_contact, Name}]).

%% find_rooms(Username) ->
%%     Names = [ Room || #chat_member{room = Room}
%%         <- mnesia:dirty_match_object(#chat_member{room = '_', uname = Username}) ],
%%     lists:append([mnesia:dirty_read(chat_room, Name) || Name <- Names]).

%% add_room(Room) when is_record(Room, chat_room) ->
%%     mnesia:transaction(fun mnesia:write/1, [Room]).

%% del_room(Name) ->
%%     mnesia:transaction(fun mnesia:delete/1, [{chat_room, Name}]).

%% store_message(Message = #mqtt_message{id = MsgId, 
%%                                       topic = <<"chat/", To/binary>>}) ->
%%     ets:insert(chat_message, {{To, MsgId}, Message, false}).

%% ack_message(ClientId, _Message = #mqtt_message{id = MsgId, 
%%                                                topic = <<"chat/", To/binary>>}) ->
%%     ets:update_element(chat_message, {To, MsgId}, {3, ClientId}).

%% find_offline_msg(To) ->
%%     {ok, lists:append(ets:match(chat_message, {{To, '_'}, '$1', false}))}.

%% sync_messages(#chat_synckey{pubsub = subscribe, 
%%                             topic = <<"chat/", To/binary>>}, Offset) ->
%%     MatchSpec = {{{To, '$1'}, '$2', '$3'}, [{'>', '$1', Offset}], ['$2']},
%%     ets:select(chat_message, [MatchSpec]);
%% sync_messages(#chat_synckey{pubsub = publish,
%%                             topic = <<"chat/", _To/binary>>}, _Offset) ->
%%     %% TODO
%%     [].

%% onunload() ->
%%     ok.
