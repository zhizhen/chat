-define(EMPTY(Field), ((Field =:= undefined) orelse (Field =:= <<>>))).
%% -record(chat_contact, {username, nick,
%%                        group = <<"friend">>,
%%                        presence = offline,
%%                        show = unavailable,
%%                        status = <<"">>,
%%                        avatar}).
-record(chat_user, {client, 
		    is_login,
		    token,
		    lastlogintime
		   }).

-record(chat_channel, {id, name,
		       type, savetype,
		       admin_uid, index,
		       uids}).

-record(chat_roster, {uname, cname}).

-record(chat_room, {name, nick,
                    topic,
                    avatar}).

-record(chat_member, {room, uname}).

-record(chat_synckey, {client   :: binary(),
                       username :: binary(),
                       pubsub   :: publish | subscribe,
                       topic    :: binary()}).

-type chat_synckey() :: #chat_synckey{}.

-record(chat_sync, {synckey :: #chat_synckey{},
                    offset  :: binary()}).

-type chat_sync() :: #chat_sync{}.


