-module(prop_bin).
-include_lib("proper/include/proper.hrl").


prop_raw_codec() ->
    ?FORALL(Val, proper_types:binary(),
            proper_lib:codec(pg_raw, [], Val)).

prop_bitstring_codec() ->
    ?FORALL(Val, proper_types:bitstring(),
            proper_lib:codec(pg_bit_string, [], Val)).
