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
    Conf = application:get_env(pg_types, timestampz_config, []),
    pg_timestamp:decode_timestamp(Bin, Conf).

type_spec() ->
    "{{Year::integer(), Month::1..12, Day::1..31}, {Hours::integer(), Minutes::integer(), Seconds::integer() | float()}, {HourOffset::integer(), MinuteOffset::pos_integer()}}".
