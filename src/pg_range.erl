-module(pg_range).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_types.hrl").

init(_Opts) ->
    {[<<"range_send">>], []}.

encode({From, To}, #type_info{pool=Pool, base_oid=BaseOid}) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, BaseOid),
    EncodedFrom = Mod:encode(From, TypeInfo),
    EncodedTo = Mod:encode(To, TypeInfo),
    [<<2:1/big-signed-unit:8>>, <<(iolist_size(EncodedFrom)):32>>,
     EncodedFrom, <<(iolist_size(EncodedTo)):32>>, EncodedTo].

decode(<<2:8, Rest/binary>>, #type_info{pool=Pool, base_oid=BaseOid}) ->
    BaseTypeInfo=#type_info{module=Mod, typlen=Len} = pg_types:lookup_type_info(Pool, BaseOid),
    <<Len:?int32, From:Len/binary, Len:?int32, To:Len/binary>> = Rest,
    DecodedFrom = Mod:decode(From, BaseTypeInfo),
    DecodedTo = Mod:decode(To, BaseTypeInfo),
    {DecodedFrom, DecodedTo}.
