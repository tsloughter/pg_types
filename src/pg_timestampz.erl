-module(pg_timestampz).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"timestamptz_send">>], []}.

encode(Timestamp, _TypeInfo) ->
    <<8:?int32, (pg_timestamp:encode_timestamp(Timestamp)):?int64>>.

decode(Bin, _TypeInfo) ->
    pg_timestamp:decode_timestamp(Bin, []).

type_spec() ->
    "{{Year::integer(), Month::1..12, Day::1..31}, {Hours::0..23, Minutes::0..59, Seconds::0..59 | float()}, {HourOffset::-15..15, MinuteOffset::0..59}}".
