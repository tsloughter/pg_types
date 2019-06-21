-module(pg_timestamp).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,

         encode_timestamp/1,
         decode_timestamp/1,
         encode_time/1]).

-define(POSTGRESQL_GD_EPOCH, 730485). % ?_value(calendar:date_to_gregorian_days({2000,1,1}))).
-define(POSTGRESQL_GS_EPOCH, 63113904000). % ?_value(calendar:datetime_to_gregorian_seconds({{2000,1,1}, {0,0,0}}))).

-define(POSTGRES_EPOC_JDATE, 2451545).
-define(POSTGRES_EPOC_USECS, 946684800000000).

-define(MINS_PER_HOUR, 60).
-define(SECS_PER_MINUTE, 60).

-define(SECS_PER_DAY, 86400.0).

-define(USECS_PER_DAY, 86400000000).
-define(USECS_PER_HOUR, 3600000000).
-define(USECS_PER_MINUTE, 60000000).
-define(USECS_PER_SEC, 1000000).

init(_Opts) ->
    {[<<"timestamp_send">>], []}.

encode(Timestamp, _TypeInfo) ->
     <<(encode_timestamp(Timestamp)):64>>.

decode(Bin, _TypeInfo) ->
    decode_timestamp(Bin).

encode_timestamp({Date, Time}) ->
    D = encode_date(Date) - ?POSTGRES_EPOC_JDATE,
    D * ?USECS_PER_DAY + encode_time(Time).

encode_date({Y, M, D}) ->
    M2 = case M > 2 of
        true ->
            M + 1;
        false ->
            M + 13
    end,
    Y2 = case M > 2 of
        true ->
            Y + 4800;
        false ->
            Y + 4799
    end,
    C = Y2 div 100,
    J1 = Y2 * 365 - 32167,
    J2 = J1 + (Y2 div 4 - C + C div 4),
    J2 + 7834 * M2 div 256 + D.

encode_time(0) ->
    0;
encode_time({H, M, S}) ->
    US = trunc(round(S * ?USECS_PER_SEC)),
    ((H * ?MINS_PER_HOUR + M) * ?SECS_PER_MINUTE) * ?USECS_PER_SEC + US;
encode_time({H, M, S}) ->
    ((H * ?MINS_PER_HOUR + M) * ?SECS_PER_MINUTE) + S.

decode_timestamp(<<16#7FFFFFFFFFFFFFFF:64/signed-integer>>) -> infinity;
decode_timestamp(<<-16#8000000000000000:64/signed-integer>>) -> '-infinity';
decode_timestamp(<<Timestamp:64/signed-integer>>) ->
    TimestampSecs = Timestamp div 1000000,
    USecs = Timestamp rem 1000000,
    decode_timestamp0(TimestampSecs, USecs).

decode_timestamp0(Secs, USecs) ->
    {Date, {Hour, Min, Secs0}} = calendar:gregorian_seconds_to_datetime(Secs + ?POSTGRESQL_GS_EPOCH),
    Secs1 = cast_datetime_usecs(Secs0, USecs),
    Time = {Hour, Min, Secs1},
    {Date, Time}.

cast_datetime_secs(Secs) ->
    Secs.

cast_datetime_usecs(Secs0, USecs) ->
    Secs1 = case USecs of
        0 -> Secs0;
        _ -> Secs0 + USecs / 1000000
    end,
    cast_datetime_secs(Secs1).
