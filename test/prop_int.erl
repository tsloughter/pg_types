-module(prop_int).
-include_lib("proper/include/proper.hrl").

prop_int2_codec() ->
    ?FORALL(Val, proper_lib:int16(),
            proper_lib:codec(pg_int2, [], Val)).

prop_int4_codec() ->
    ?FORALL(Val, proper_lib:int32(),
            proper_lib:codec(pg_int4, [], Val)).

prop_int8_codec() ->
    ?FORALL(Val, proper_lib:int64(),
            proper_lib:codec(pg_int8, [], Val)).
