-module(prop_float).
-include_lib("proper/include/proper.hrl").

%% TODO: fix truncation problem
%% prop_float4_codec() ->
%%     ?FORALL(Val, proper_types:float(-999.99, 990.99),
%%             proper_lib:codec(pg_float4, [], Val, fun canonical_float4/1)).
%%
%% canonical_float4(N) ->
%%     round(N * 100).

prop_float8_codec() ->
    ?FORALL(Val, proper_types:float(),
            proper_lib:codec(pg_float8, [], Val)).
