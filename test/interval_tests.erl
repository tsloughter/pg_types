-module(interval_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pg_types.hrl").

default_basic_test() ->
    ?assertEqual(
        {interval, {{3, 2, 1}, 5, 0}},
        proper_lib:encode_decode(pg_interval, [], {interval, {{3, 2, 1}, 5, 0}})
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

raw_basic_test() ->
    ?assertEqual(
        {interval, 10921000000, 5, 0},
        proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, 10921000000, 5, 0})
    ).

raw_large_hours_test() ->
    ?assertEqual(
        {interval, 360000000000, 0, 0},
        proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, 360000000000, 0, 0})
    ).

raw_negative_test() ->
    ?assertEqual(
        {interval, -3600000000, 0, 0},
        proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, -3600000000, 0, 0})
    ).

raw_negative_days_test() ->
    ?assertEqual(
        {interval, 0, -5, 0},
        proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, 0, -5, 0})
    ).

raw_zero_test() ->
    ?assertEqual(
        {interval, 0, 0, 0},
        proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, 0, 0, 0})
    ).

raw_encode_from_old_format_test() ->
    Decoded = proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {interval, {{3, 2, 1}, 5, 0}}),
    ?assertEqual({interval, 10921000000, 5, 0}, Decoded).

raw_encode_bare_tuple_test() ->
    Decoded = proper_lib:encode_decode(pg_interval, #{interval_config => raw}, {{3, 2, 1}, 5, 0}),
    ?assertEqual({interval, 10921000000, 5, 0}, Decoded).
