-module(pg_range).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").
-include("pg_types.hrl").

init(_Opts) ->
    {[<<"range_send">>], []}.

encode({From, To}, #type_info{pool=Pool, base_oid=BaseOid}) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, BaseOid),
    Data = [Mod:encode(From, TypeInfo), Mod:encode(To, TypeInfo)],
    [<<(iolist_size(Data) + 1):?int32, 2:?int8>>, Data].

decode(<<2:?int8, Rest/binary>>, #type_info{pool=Pool, base_oid=BaseOid}) ->
    BaseTypeInfo=#type_info{module=Mod, typlen=Len} = pg_types:lookup_type_info(Pool, BaseOid),
    <<Len:?int32, From:Len/binary, Len:?int32, To:Len/binary>> = Rest,
    DecodedFrom = Mod:decode(From, BaseTypeInfo),
    DecodedTo = Mod:decode(To, BaseTypeInfo),
    {DecodedFrom, DecodedTo}.
