-module(interval_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pg_types.hrl").

default_basic_test() ->
    ?assertEqual(
        {interval, {{3, 2, 1}, 5, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{3, 2, 1}, 5, 0}})
    ).

default_large_hours_test() ->
    ?assertEqual(
        {interval, {{100, 0, 0}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{100, 0, 0}, 0, 0}})
    ).

default_negative_hours_test() ->
    ?assertEqual(
        {interval, {{-1, 0, 0}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{-1, 0, 0}, 0, 0}})
    ).

default_negative_seconds_test() ->
    ?assertEqual(
        {interval, {{0, 0, -1}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, -1}, 0, 0}})
    ).

default_negative_days_test() ->
    ?assertEqual(
        {interval, {{0, 0, 0}, -5, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, 0}, -5, 0}})
    ).

default_negative_months_test() ->
    ?assertEqual(
        {interval, {{0, 0, 0}, 0, -3}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, 0}, 0, -3}})
    ).

default_float_seconds_test() ->
    ?assertEqual(
        {interval, {{0, 0, 1.5}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, 1.5}, 0, 0}})
    ).

default_negative_float_seconds_test() ->
    ?assertEqual(
        {interval, {{0, 0, -1.5}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, -1.5}, 0, 0}})
    ).

default_zero_test() ->
    ?assertEqual(
        {interval, {{0, 0, 0}, 0, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{0, 0, 0}, 0, 0}})
    ).

default_complex_test() ->
    ?assertEqual(
        {interval, {{3, 2, 1}, 4, 77}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{3, 2, 1}, 4, 77}})
    ).

decode_100_hours_test() ->
    Usecs = 360000000000,
    Bin = <<Usecs:64/big-signed, 0:32/big-signed, 0:32/big-signed>>,
    ?assertEqual({interval, {{100, 0, 0}, 0, 0}}, pg_interval:decode(Bin, #type_info{})).

decode_negative_1_hour_test() ->
    Usecs = -3600000000,
    Bin = <<Usecs:64/big-signed, 0:32/big-signed, 0:32/big-signed>>,
    ?assertEqual({interval, {{-1, 0, 0}, 0, 0}}, pg_interval:decode(Bin, #type_info{})).

decode_negative_1_second_test() ->
    Usecs = -1000000,
    Bin = <<Usecs:64/big-signed, 0:32/big-signed, 0:32/big-signed>>,
    ?assertEqual({interval, {{0, 0, -1}, 0, 0}}, pg_interval:decode(Bin, #type_info{})).

decode_1000h_30m_15s_test() ->
    Usecs = 3601815000000,
    Bin = <<Usecs:64/big-signed, 0:32/big-signed, 0:32/big-signed>>,
    ?assertEqual({interval, {{1000, 30, 15}, 0, 0}}, pg_interval:decode(Bin, #type_info{})).

decode_negative_1_5_seconds_test() ->
    Usecs = -1500000,
    Bin = <<Usecs:64/big-signed, 0:32/big-signed, 0:32/big-signed>>,
    ?assertEqual({interval, {{0, 0, -1.5}, 0, 0}}, pg_interval:decode(Bin, #type_info{})).
