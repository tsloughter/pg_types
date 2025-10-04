-module(pg_timestamp).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0,
         encode_timestamp/1,
         decode_timestamp/2]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

-define(POSTGRESQL_GS_EPOCH, 63113904000). % calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}).
-define(UNIX_GS_EPOCH, 62167219200). % calendar:datetime_to_gregorian_seconds({{19790,1,1}, {0,0,0}}).
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(MILLION, 1_000_000).

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

% A timestamp with a positive offset is a time in the future, compared to UTC,
% and therefore you need to subtract the hour and minutes to generate a UTC
% time. Please see 'test/timestamptz_tests.erl' for some examples.
encode_timestamp(SystemTime) when is_integer(SystemTime) ->
    SystemTime + ((?UNIX_GS_EPOCH - ?POSTGRESQL_GS_EPOCH) * ?MILLION);
encode_timestamp({Date, Time, {HourOffset, MinuteOffset}}) when MinuteOffset >= 0 ->
    Sign = determine_sign(HourOffset),
    OffsetSecs = abs(HourOffset) * ?SECONDS_PER_HOUR + MinuteOffset * ?SECONDS_PER_MINUTE,
    datetime_to_micros(Date, Time, Sign * OffsetSecs);
encode_timestamp({Date, Time}) ->
    datetime_to_micros(Date, Time, 0);
encode_timestamp(infinity) ->
    16#7FFFFFFFFFFFFFFF;
encode_timestamp('-infinity') ->
    -16#8000000000000000.

datetime_to_micros({Year, Month, Day}, {Hours, Minutes, Seconds}, Offset) ->
    %% if Seconds are integer(), we'll just get zero micros
    IntegerSeconds = trunc(Seconds),
    USecs = trunc((Seconds - IntegerSeconds) * ?MILLION),
    %% note that we want to avoid rebuilding the time tuple here
    Secs = ?SECONDS_PER_DAY * calendar:date_to_gregorian_days(Year, Month, Day) + time_to_seconds(Hours, Minutes, IntegerSeconds) - ?POSTGRESQL_GS_EPOCH,
    ((Secs + Offset) * ?MILLION) + USecs.

%% the calendar module only has the arity-1 version of this function
time_to_seconds(H, M, S) when is_integer(H), is_integer(M), is_integer(S) ->
    H * ?SECONDS_PER_HOUR + M * ?SECONDS_PER_MINUTE + S.


-spec decode_timestamp(binary(), config() | []) -> datetime().
decode_timestamp(<<16#7FFFFFFFFFFFFFFF:?int64>>, _) -> infinity;
decode_timestamp(<<-16#8000000000000000:?int64>>, _) -> '-infinity';
decode_timestamp(<<Timestamp:?int64>>, float_system_time_seconds) ->
    %% Note: We do this instead of just dividing by 1000000 to not end up with float inaccuracies
    USecs = Timestamp rem ?MILLION,
    (Timestamp div ?MILLION) + ?POSTGRESQL_GS_EPOCH - ?UNIX_GS_EPOCH + (USecs / ?MILLION);
decode_timestamp(<<Timestamp:?int64>>, integer_system_time_seconds) ->
    (Timestamp div ?MILLION) + ?POSTGRESQL_GS_EPOCH - ?UNIX_GS_EPOCH;
decode_timestamp(<<Timestamp:?int64>>, integer_system_time_microseconds) ->
    Timestamp + (?POSTGRESQL_GS_EPOCH - ?UNIX_GS_EPOCH) * ?MILLION;
decode_timestamp(<<Timestamp:?int64>>, _) ->
    TimestampSecs = Timestamp div ?MILLION,
    USecs = Timestamp rem ?MILLION,
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
    Secs + (USecs / ?MILLION).

% When the hour offset is positive, you are in the future and therefore
% need to subtract the hours to get to UTC.
determine_sign(HourOffset) when HourOffset >= 0 ->
    -1;
determine_sign(_) ->
    1.
