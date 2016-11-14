-define(EMPTY(Field), ((Field =:= undefined) orelse (Field =:= <<>>))).
-record(slimchat_contact, {username, nick,
                           group = <<"friend">>,
                           presence = offline,
                           show = unavailable,
                           status = <<"">>,
                           avatar}).

-record(slimchat_roster, {uname, cname}).

-record(slimchat_room, {name, nick,
                        topic,
                        avatar}).

-record(slimchat_member, {room, uname}).

-record(slimchat_synckey, {client   :: binary(),
                           username :: binary(),
                           pubsub   :: publish | subscribe,
                           topic    :: binary()}).

-type slimchat_synckey() :: #slimchat_synckey{}.

-record(slimchat_sync, {synckey :: #slimchat_synckey{},
                        offset  :: pos_integer()}).

-type slimchat_sync() :: #slimchat_sync{}.


