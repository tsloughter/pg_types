-module(prop_timestamp).
-include_lib("proper/include/proper.hrl").

prop_int_sec_codec() ->
    ?FORALL(Val, int_timestamp(),
            proper_lib:codec(pg_timestamp, [], Val)).

prop_tz_int_sec_codec() ->
    ?FORALL(Val, int_timestamp(),
            proper_lib:codec(pg_timestampz, [], Val)).

prop_tz_offset_codec() ->
    ?FORALL(Val, utc_offset_timestamp(),
            proper_lib:codec(pg_timestampz, [], Val, apply_offset(Val))).

apply_offset({{Year, Month, Day}, {Hour, Minute, Second}, {HourOffset, MinuteOffset}}) ->
    Sign = case HourOffset >= 0 of
        true -> -1;
        false -> 1
    end,
    OffsetHours = calendar:time_to_seconds({abs(HourOffset), 0, 0}),
    OffsetMinutes = calendar:time_to_seconds({0, MinuteOffset, 0}),
    DatetimeSeconds = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
    calendar:gregorian_seconds_to_datetime(DatetimeSeconds + OffsetHours * Sign + OffsetMinutes * Sign).

int_timestamp() ->
    {proper_lib:date_gen(), proper_lib:int_time_gen()}.

utc_offset_timestamp() ->
    {proper_lib:date_gen(), proper_lib:int_time_gen(), proper_lib:utc_offset_gen()}.
