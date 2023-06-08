-module(prop_date).
-include_lib("proper/include/proper.hrl").

prop_codec() ->
    ?FORALL(Val, proper_lib:date_gen(),
            proper_lib:codec(pg_date, [], Val)).
