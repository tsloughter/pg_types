-module(prop_hstore).
-include_lib("proper/include/proper.hrl").

prop_binary_codec() ->
    ?FORALL(Val, bin_map(),
            proper_lib:codec(pg_hstore, [], Val)).

bin_map() ->
    ?LET(KV,
         proper_types:list(
           {proper_types:binary(),
            proper_types:binary()}
          ),
         maps:from_list(KV)).

prop_strkey_codec() ->
    ?FORALL(Val, strkey_map(),
            proper_lib:codec(pg_hstore, [], Val, fun canonical_keys/1)).

strkey_map() ->
    ?LET(KV,
         proper_types:list(
           {proper_types:oneof(
             [proper_types:atom(),
              proper_types:list(proper_types:byte()),
              proper_types:binary()]),
           proper_types:binary()}
          ),
         maps:from_list(KV)).

canonical_keys(Map) ->
    maps:fold(
      fun(K, V, Acc) when is_list(K) ->
              Acc#{list_to_binary(K) => V};
         (K, V, Acc) when is_atom(K) ->
              Acc#{atom_to_binary(K, utf8) => V};
         (K, V, Acc) ->
              Acc#{K => V}
      end, #{}, Map).
