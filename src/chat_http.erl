-module(chat_http).

-export([handle_request/1]).

-define(APIVER, "/v1").

handle_request(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    lager:info("HTTP ~s ~s", [Method, Path]),
    handle_request(Method, Path, Req).

%% Login
handle_request('POST', ?APIVER ++ "/login", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Username = g("username", Params),
    Password = g("password", Params),
    case chat_auth:check(Username, Password) of
        ok ->
            jsonReply(Req, [{status, ok}]);
        {error, Error} ->
            jsonReply(Req, [{status, error}, {error, Error}])
    end;

%% Online
handle_request('POST', ?APIVER ++ "/online", Req) ->
    Params = mochiweb_request:parse_post(Req),
    Username = list_to_binary(g("username", Params)),
    Contacts = chat_backend:find_contacts(Username),
    Rooms = chat_backend:find_rooms(Username),
    Response = [{success, true},
                {server_time, chat_misc:now_to_secs()},
                {ticket, chat_ticket:token()},
                {broker, list_to_binary(chat:broker())},
                {buddies, [chat_contact:to_list(Contact) || Contact <- Contacts]},
                {rooms, [chat_room:to_list(Room) || Room <- Rooms]},
                {user, [{id, Username}, {nick, Username},
                        {presence, online}, {show, available}]}],
    jsonReply(Req, Response);

%% Contacts
handle_request('GET', ?APIVER ++ "/contacts", Req) ->
    Req:ok({"text/plain", <<"[]">>});

%% 404
handle_request(Method, Path, Req) ->
    lager:error("HTTP Bad Request: ~s ~s", [Method, Path]),
    Req:not_found().

g(Name, Params) ->
    proplists:get_value(Name, Params).

jsonReply(Req, Data) ->
    Json = mochijson2:encode(Data),
    Req:ok({"application/json", Json}).
