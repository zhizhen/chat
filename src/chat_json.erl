-module(chat_json).

-export([encode/1, decode/1]).

%% encode maps to json
encode(Data) ->
    iolist_to_binary(mochijson2:encode(Data)).

%% @doc decode json to maps
decode(Data) ->
    {ok, structs_to_maps(mochijson2:decode(Data))}.

structs_to_maps({struct, Props}) when is_list(Props) ->
    
    lists:foldl(
        fun({Key, Val}, Map) ->
		
            Map#{Key => structs_to_maps(Val)}
        end,
        #{},
        Props
     );
structs_to_maps(Vals) when is_list(Vals) ->    
    lists:map(
        fun(Val) ->            
		structs_to_maps(Val)
        end,
      Vals
     );
structs_to_maps(Val) ->
    Val.
