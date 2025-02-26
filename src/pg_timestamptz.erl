-module(pg_timestamptz).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(#{timestamptz_config := Config}) ->
    {[<<"timestamptz_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, timestamptz_config, []),
    {[<<"timestamptz_send">>], Config}.

encode(Timestamp, _TypeInfo) ->
    <<8:?int32, (pg_timestamp:encode_timestamp(Timestamp)):?int64>>.

decode(Bin, #type_info{config=Config}) ->
    pg_timestamp:decode_timestamp(Bin, Config).

type_spec() ->
    "{{Year::integer(), Month::1..12, Day::1..31}, {Hours::integer(), Minutes::integer(), Seconds::integer() | float()}, {HourOffset::integer(), MinuteOffset::pos_integer()}}".
