-module(pg_inet).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-define(INET, 2).
-define(INET6, 3).
-define(IP_SIZE, 4).
-define(IP6_SIZE, 16).
-define(MAX_IP_MASK, 32).
-define(MAX_IP6_MASK, 128).

init(_Opts) ->
    {[<<"inet_send">>, <<"cidr_send">>], []}.

encode({{_, _, _, _} = IP, Mask}, _) ->
    Bin = list_to_binary(tuple_to_list(IP)),
    <<8:?int32, ?INET, Mask:8, 1, ?IP_SIZE, Bin/binary>>;
encode({{_, _, _, _, _, _, _, _} = IP, Mask}, _) ->
    Bin = << <<X:16>> || X <- tuple_to_list(IP) >>,
    <<20:?int32, ?INET6, Mask:8, 1, ?IP6_SIZE, Bin/binary>>;
encode({_, _, _, _} = IP, _) ->
    Bin = list_to_binary(tuple_to_list(IP)),
    <<8:?int32, ?INET, ?MAX_IP_MASK, 0, ?IP_SIZE, Bin/binary>>;
encode({_, _, _, _, _, _, _, _} = IP, _) ->
    Bin = << <<X:16>> || X <- tuple_to_list(IP) >>,
    <<20:?int32, ?INET6, ?MAX_IP6_MASK, 0, ?IP6_SIZE, Bin/binary>>.

decode(<<?INET, Mask:8, 1, ?IP_SIZE, Bin/binary>>, _) ->
    {list_to_tuple(binary_to_list(Bin)), Mask};
decode(<<?INET6, Mask:8, 1, ?IP6_SIZE, Bin/binary>>, _) ->
    {list_to_tuple([X || <<X:16>> <= Bin]), Mask};
decode(<<?INET, ?MAX_IP_MASK, 0, ?IP_SIZE, Bin/binary>>, _) ->
    list_to_tuple(binary_to_list(Bin));
decode(<<?INET6, ?MAX_IP6_MASK, 0, ?IP6_SIZE, Bin/binary>>, _) ->
    list_to_tuple([X || <<X:16>> <= Bin]).
