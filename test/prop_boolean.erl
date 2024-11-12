-module(prop_boolean).
-include_lib("proper/include/proper.hrl").


prop_codec() ->
    ?FORALL(Val, proper_types:boolean(),
            proper_lib:codec(pg_boolean, [], Val)).
