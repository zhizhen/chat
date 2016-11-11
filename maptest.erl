-module(maptest).
-export([test/0, double/1]).

double(N) ->
    2 * N.

test() ->
    List = [1,2,3,4],
    lists:map(fun ?MODULE:double/1, List).
