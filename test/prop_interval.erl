-module(prop_interval).
-include_lib("proper/include/proper.hrl").
-include("pg_types.hrl").

prop_raw_format_codec() ->
    ?FORALL(Val, raw_interval_gen(),
            proper_lib:codec(pg_interval, #{interval_config => raw}, Val)).

raw_interval_gen() ->
    {interval, proper_lib:int64(), proper_lib:int32(), proper_lib:int32()}.
