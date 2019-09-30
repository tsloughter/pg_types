-module(proper_lib).

-export([codec/3, codec/4]).

-export([int16/0, int32/0, int64/0,
         date_gen/0, int_time_gen/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("pg_types.hrl").

codec(Mod, Opts, Data) ->
    codec(Mod, Opts, Data, fun(V) -> V end).

codec(Mod, Opts, Data, Canonical) ->
    {_, Config} = Mod:init(Opts),
    TypeInfo = #type_info{config = Config},
    <<Size:32, Encoded:Size/binary>> = iolist_to_binary(Mod:encode(Data, TypeInfo)),
    ?assertEqual(Canonical(Data), Canonical(Mod:decode(Encoded, TypeInfo)),
                 {Mod, Opts}),
    true.


%%
%% Generators
%%

int16() ->
    proper_types:integer(-32768, 32256).

int32() ->
    proper_types:integer(-2147483648, 2130706432).

int64() ->
    proper_types:integer(-9223372036854775808, 9151314442816847872).

date_gen() ->
    ?SUCHTHAT(
       Date,
       {proper_types:integer(-9999, 9999),
        proper_types:integer(1, 12),
        proper_types:integer(1, 31)},
       calendar:valid_date(Date)).

int_time_gen() ->
    {proper_types:integer(0, 23),
     proper_types:integer(0, 59),
     proper_types:integer(0, 59)}.
