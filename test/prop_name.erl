-module(prop_name).
-include_lib("proper/include/proper.hrl").


prop_codec() ->
    ?FORALL(Val, bin64(),
            proper_lib:codec(pg_name, [], Val)).


bin64() ->
    ?LET(Size,
         proper_types:integer(0, 63),
         proper_types:binary(Size)).
