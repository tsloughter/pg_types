-module(pg_time).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0,
         encode_time/1,
         decode_time/1]).

-include("pg_protocol.hrl").

-define(MINS_PER_HOUR, 60).
-define(SECS_PER_MINUTE, 60).
-define(USECS_PER_SEC, 1000000).

init(_Opts) ->
    {[<<"time_send">>], []}.

encode(Time={Hours, Minutes, Seconds}, _TypeInfo) when is_integer(Hours) ,
                                                       is_integer(Minutes) ,
                                                       (is_integer(Seconds) orelse is_float(Seconds)) ->
    <<8:?int32, (encode_time(Time)):?int64>>.

decode(Bin, _TypeInfo) ->
    decode_time(Bin).

type_spec() ->
    "{Hours::integer(), Minutes::integer(), Seconds::integer() | float()} | 0".

%%

decode_time(<<Time:?int64>>) ->
    Seconds = Time div 1000000,
    USecs = Time rem 1000000,
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

encode_time(0) ->
    0;
encode_time({H, M, S}) when is_integer(S) ->
    US = S * ?USECS_PER_SEC,
    ((H * ?MINS_PER_HOUR + M) * ?SECS_PER_MINUTE) * ?USECS_PER_SEC + US;
encode_time({H, M, S}) when is_float(S) ->
    US = trunc(round(S * ?USECS_PER_SEC)),
    ((H * ?MINS_PER_HOUR + M) * ?SECS_PER_MINUTE) * ?USECS_PER_SEC + US.
