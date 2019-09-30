-module(prop_inet).
-include_lib("proper/include/proper.hrl").

prop_ip4_codec() ->
    ?FORALL(Val, ipv4(),
            proper_lib:codec(pg_inet, [], Val)).

prop_ip6_codec() ->
    ?FORALL(Val, ipv6(),
            proper_lib:codec(pg_inet, [], Val)).

prop_ip4_cidr_codec() ->
    ?FORALL(Val, {ipv4(), proper_types:integer(0, 32)},
            proper_lib:codec(pg_inet, [], Val)).

prop_ip6_cidr_codec() ->
    ?FORALL(Val, {ipv6(), proper_types:integer(4, 128)},
            proper_lib:codec(pg_inet, [], Val)).


ipv4() ->
    {proper_types:byte(),
     proper_types:byte(),
     proper_types:byte(),
     proper_types:byte()}.

ipv6() ->
    E = proper_types:integer(0, 65535),
    {E, E, E, E,
     E, E, E, E}.
