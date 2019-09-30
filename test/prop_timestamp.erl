-module(prop_timestamp).
-include_lib("proper/include/proper.hrl").

prop_int_sec_codec() ->
    ?FORALL(Val, int_timestamp(),
            proper_lib:codec(pg_timestamp, [], Val)).

prop_tz_int_sec_codec() ->
    ?FORALL(Val, int_timestamp(),
            proper_lib:codec(pg_timestampz, [], Val)).

int_timestamp() ->
    {proper_lib:date_gen(), proper_lib:int_time_gen()}.
