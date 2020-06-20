-module(pg_timestamp).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0,
         encode_timestamp/1,
         decode_timestamp/1,
         decode_timestamp/2]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

-define(POSTGRESQL_GD_EPOCH, 730485). % calendar:date_to_gregorian_days({2000,1,1}).
-define(POSTGRESQL_GS_EPOCH, 63113904000). % calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}).

%% config options allow timestamp to return seconds since epoch as a float or integer
%% instead of the datetime tuple format
-type config() :: float_system_time_seconds |
                  integer_system_time_seconds |
                  integer_system_time_microseconds.

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59 | float().
-type time() :: {hour(), minute(), second()}.
-type datetime() :: calendar:datetime() | {calendar:date(), time()}.

-export_type([datetime/0,
              time/0]).

init(#{timestamp_config := Config}) ->
    {[<<"timestamp_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, timestamp_config, []),
    {[<<"timestamp_send">>], Config}.

encode(Timestamp, _TypeInfo) ->
    <<8:?int32, (encode_timestamp(Timestamp)):?int64>>.

decode(Bin, #type_info{config=Config}) ->
    decode_timestamp(Bin, Config).

type_spec() ->
    "infinity | '-infinity' | {{Year::integer(), Month::1..12, Day::1..31}, {Hours::integer(), Minutes::integer(), Seconds::integer() | float()}} | 0".

encode_timestamp(infinity) ->
    16#7FFFFFFFFFFFFFFF;
encode_timestamp('-infinity') ->
    -16#8000000000000000;
encode_timestamp(Datetime={{_, _, _}, {_, _, Seconds}}) when is_integer(Seconds)->
    Secs = calendar:datetime_to_gregorian_seconds(Datetime) - ?POSTGRESQL_GS_EPOCH,
    Secs * 1000000;
encode_timestamp({{Year, Month, Day}, {Hours, Minutes, Seconds}}) when is_float(Seconds)->
    IntegerSeconds = trunc(Seconds),
    US = trunc((Seconds - IntegerSeconds) * 1000000),
    Secs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                   {Hours, Minutes, IntegerSeconds}}) - ?POSTGRESQL_GS_EPOCH,
    (Secs * 1000000) + US;
encode_timestamp(SystemTime) when is_integer(SystemTime) ->
    SystemTime + ((62167219200 - ?POSTGRESQL_GS_EPOCH) * 1000000).

-spec decode_timestamp(binary()) -> datetime().
decode_timestamp(Bin) ->
    decode_timestamp(Bin, []).

-spec decode_timestamp(binary(), config() | []) -> datetime().
decode_timestamp(<<16#7FFFFFFFFFFFFFFF:?int64>>, _) -> infinity;
decode_timestamp(<<-16#8000000000000000:?int64>>, _) -> '-infinity';
decode_timestamp(<<Timestamp:?int64>>, float_system_time_seconds) ->
    %% Note: We do this instead of just dividing by 1000000 to not end up with float inaccuracies
    USecs = Timestamp rem 1000000,
    ((Timestamp div 1000000) + ?POSTGRESQL_GS_EPOCH) - 62167219200 + (USecs / 1000000);
decode_timestamp(<<Timestamp:?int64>>, integer_system_time_seconds) ->
    ((Timestamp div 1000000) + ?POSTGRESQL_GS_EPOCH) - 62167219200;
decode_timestamp(<<Timestamp:?int64>>, integer_system_time_microseconds) ->
    Timestamp + (?POSTGRESQL_GS_EPOCH * 1000000) - (62167219200 * 1000000);
decode_timestamp(<<Timestamp:?int64>>, _) ->
    TimestampSecs = Timestamp div 1000000,
    USecs = Timestamp rem 1000000,
    decode_timestamp0(TimestampSecs, USecs).

decode_timestamp0(Secs, USecs) ->
    {Date, {Hour, Min, Seconds}} = calendar:gregorian_seconds_to_datetime(Secs + ?POSTGRESQL_GS_EPOCH),
    Seconds1 = add_usecs(Seconds, USecs),
    Time = {Hour, Min, Seconds1},
    {Date, Time}.

add_usecs(Secs, 0) ->
    %% leave seconds as an integer if there are no usecs
    Secs;
add_usecs(Secs, USecs) ->
    Secs + (USecs / 1000000).
