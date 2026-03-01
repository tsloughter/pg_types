-module(prop_interval).
-include_lib("proper/include/proper.hrl").
-include("pg_types.hrl").

prop_old_format_codec() ->
    ?FORALL(Val, old_interval_gen(),
            proper_lib:codec(pg_interval, [], Val, fun(V) -> to_wire(V, []) end)).

prop_raw_format_codec() ->
    ?FORALL(Val, raw_interval_gen(),
            proper_lib:codec(pg_interval, #{interval_config => raw}, Val)).

old_interval_gen() ->
    {interval, {interval_time_gen(),
                proper_lib:int32(),
                proper_lib:int32()}}.

interval_time_gen() ->
    {proper_types:integer(-10000, 10000),
     proper_types:integer(0, 59),
     proper_types:integer(0, 59)}.

raw_interval_gen() ->
    {interval, proper_lib:int64(), proper_lib:int32(), proper_lib:int32()}.

to_wire(Val, Opts) ->
    {_, Config} = pg_interval:init(Opts),
    TypeInfo = #type_info{config = Config},
    pg_interval:encode(Val, TypeInfo).
