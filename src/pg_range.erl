-module(pg_range).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").
-include("pg_types.hrl").

-define(RANGE_EMPTY, 16#01).
-define(RANGE_LB_INC, 16#02).
-define(RANGE_UB_INC, 16#04).
-define(RANGE_LB_INF, 16#08).
-define(RANGE_UB_INF, 16#10).

init(_Opts) ->
    {[<<"range_send">>], []}.

encode({{LowerInclusive, From}, {UpperInclusive, To}}, #type_info{pool=Pool, base_oid=BaseOid})
  when is_atom(LowerInclusive) ; is_atom(UpperInclusive) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, BaseOid),

    Flags = case {LowerInclusive, UpperInclusive} of
                {true, true} ->
                  ?RANGE_LB_INC bor ?RANGE_UB_INC;
                {false, true} ->
                  ?RANGE_UB_INC;
                {true, false} ->
                  ?RANGE_LB_INC;
                {false, false} ->
                  0
            end,

    {Data, Flags1} = case {From, To} of
                         {unbound, unbound} ->
                             {[], Flags bor ?RANGE_LB_INF bor ?RANGE_UB_INF};
                         {unbound, To} ->
                             {[Mod:encode(To, TypeInfo)], Flags bor ?RANGE_LB_INF};
                         {From, unbound} ->
                             {[Mod:encode(From, TypeInfo)], Flags bor ?RANGE_UB_INF};
                         {From, To} ->
                             {[Mod:encode(From, TypeInfo), Mod:encode(To, TypeInfo)], Flags}
                     end,
    [<<(iolist_size(Data) + 1):?int32, Flags1:?int8>>, Data];
encode({From, To}, TypeInfo) ->
    encode({{true, From}, {true, To}}, TypeInfo).

decode(<<?RANGE_EMPTY:?int8>>, _) ->
    empty;
decode(<<Flags:?int8, Rest/binary>>, #type_info{pool=Pool, base_oid=BaseOid}) ->
    BaseTypeInfo=#type_info{module=Mod, typlen=Len} = pg_types:lookup_type_info(Pool, BaseOid),

    {LowerBound, Rest1} = case 0 =/= Flags band ?RANGE_LB_INF of
                              true -> {unbound, Rest};
                              _ ->
                                  <<Len:?int32, From:Len/binary, More/binary>> = Rest,
                                  {Mod:decode(From, BaseTypeInfo), More}
                          end,
    UpperBound = case 0 =/= Flags band ?RANGE_UB_INF of
                     true ->
                         unbound;
                     _ ->
                         <<L:?int32, To:L/binary>> = Rest1,
                         Mod:decode(To, BaseTypeInfo)
                 end,
    LowerInclusive = (Flags band ?RANGE_LB_INC) =/= 0,
    UpperInclusive = (Flags band ?RANGE_UB_INC) =/= 0,
    {{LowerInclusive, LowerBound}, {UpperInclusive, UpperBound}}.
