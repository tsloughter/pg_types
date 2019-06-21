-module(pg_time).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,

         decode_time/1]).

init(_Opts) ->
    {[<<"time_send">>], []}.

encode(Time, _TypeInfo) ->
    <<(pg_timestamp:encode_time(Time)):64>>.

decode(Bin, _TypeInfo) ->
    decode_time(Bin).

decode_time(<<Time:64/signed-integer>>) ->
    Seconds = Time div 1000000,
    USecs = Time rem 1000000,
    decode_time0(Seconds, USecs);
decode_time(<<Time:64/float>>) ->
    Seconds = trunc(Time),
    USecs = round((Time - Seconds) * 1000000),   % Maximum documented PostgreSQL precision is usec.
    decode_time0(Seconds, USecs).

decode_time0(Seconds, USecs) ->
    {Hour, Min, Secs0} = calendar:seconds_to_time(Seconds),
    Secs1 = cast_datetime_usecs(Secs0, USecs),
    {Hour, Min, Secs1}.

cast_datetime_secs(Secs) ->
    Secs.

cast_datetime_usecs(Secs0, USecs) ->
    Secs1 = case USecs of
        0 -> Secs0;
        _ -> Secs0 + USecs / 1000000
    end,
    cast_datetime_secs(Secs1).
