-module(prop_time).
-include_lib("proper/include/proper.hrl").

prop_int_sec_codec() ->
    ?FORALL(Val, proper_lib:int_time_gen(),
            proper_lib:codec(pg_time, [], Val)).
