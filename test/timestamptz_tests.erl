-module(timestamptz_tests).

-include_lib("eunit/include/eunit.hrl").

negative_offset_timestamptz_test() ->
    ?assertEqual(
        proper_lib:encode_decode(
            pg_timestampz,
            [],
            {{2024, 3, 1}, {1, 0, 0}, {-5, 15}}
        ),
        {{2024, 3, 1}, {6, 15, 0}}
    ).

positive_offset_timestamptz_test() ->
    ?assertEqual(
        proper_lib:encode_decode(
            pg_timestampz,
            [],
            {{2024, 3, 1}, {1, 0, 0}, {5, 15}}
        ),
        {{2024, 2, 29}, {19, 45, 0}}
    ).

no_offset_timestamptz_test() ->
    ?assertEqual(
        proper_lib:encode_decode(
            pg_timestampz,
            [],
            {{2024, 3, 1}, {1, 0, 0}, {0, 0}}
        ),
        {{2024, 3, 1}, {1, 0, 0}}
    ).
