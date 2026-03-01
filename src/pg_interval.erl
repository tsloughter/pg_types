-module(pg_interval).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

-define(USECS_PER_SEC, 1000000).
-define(SECS_PER_MINUTE, 60).
-define(SECS_PER_HOUR, 3600).

init(_Opts) ->
    {[<<"interval_send">>], []}.

encode({interval, {T, D, M}}, _) ->
    <<16:?int32, (pg_time:encode_time(T)):?int64, D:?int32, M:?int32>>;
encode({T, D, M}, _) ->
    <<16:?int32, (pg_time:encode_time(T)):?int64, D:?int32, M:?int32>>.

decode(<<Microseconds:?int64, Days:?int32, Months:?int32>>, _) ->
    {interval, {decode_interval_time(Microseconds), Days, Months}}.

%% PostgreSQL wire format: int64 microseconds (signed), int32 days (signed), int32 months (signed).
%% Microseconds are decomposed into {Hours, Minutes, Seconds} using signed div/rem
%% to correctly handle negative values and values exceeding 24 hours.
decode_interval_time(0) ->
    {0, 0, 0};
decode_interval_time(Microseconds) ->
    TotalSeconds = Microseconds div ?USECS_PER_SEC,
    Remainder = Microseconds rem ?USECS_PER_SEC,
    H = TotalSeconds div ?SECS_PER_HOUR,
    Rem1 = TotalSeconds rem ?SECS_PER_HOUR,
    M = Rem1 div ?SECS_PER_MINUTE,
    S0 = Rem1 rem ?SECS_PER_MINUTE,
    case Remainder of
        0 -> {H, M, S0};
        _ -> {H, M, S0 + Remainder / ?USECS_PER_SEC}
    end.

type_spec() ->
    "{interval {{Hours::integer(), Minutes::integer(), "
    "Seconds::integer()}, Days::integer(), Months::integer()}}".
