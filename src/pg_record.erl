-module(pg_record).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"record_send">>], []}.

encode(Tuple, #type_info{pool=Pool,
                         comp_oids=Oids}) ->
    Data = encode_tuple(tuple_to_list(Tuple), Oids, Pool),
    [<<(iolist_size(Data) + 4):?int32, (erlang:tuple_size(Tuple)):?int32>> | Data].

decode(<<Count:?int32, Data/binary>>, #type_info{pool=Pool,
                                                 comp_oids=[]}) ->
    Types = [],
    decode_tuple_no_oids(Data, Count, Pool, Types);
decode(<<_:?int32, Data/binary>>, #type_info{pool=Pool,
                                             comp_oids=Oids}) ->
    Types = [],
    decode_tuple(Data, Oids, Pool, Types).
%%

encode_tuple(Elems, Oids, Pool) ->
    encode_tuple(Elems, Oids, Pool, []).

encode_tuple([], [], _, Acc) ->
    Acc;
encode_tuple([null | T], [Oid | Oids], Pool, Acc) ->
    EncodedElem = <<-1:?int32>>,
    encode_tuple(T, Oids, Pool, Acc ++ [<<Oid:?int32>>, EncodedElem]);
encode_tuple([H | T], [Oid | Oids], Pool, Acc) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, Oid),
    EncodedElem = Mod:encode(H, TypeInfo),
    encode_tuple(T, Oids, Pool, Acc ++ [<<Oid:?int32>>, EncodedElem]).

decode_tuple(Data, Oids, Pool, Types) ->
    decode_tuple(Data, Oids, Pool, Types, []).

decode_tuple(<<>>, [], _, _Types, Acc) ->
    list_to_tuple(lists:reverse(Acc));
decode_tuple(<<_Oid:?int32, -1:?int32, RestData/binary>>,
             [_Oid | RestOids], Pool, Types, Acc) ->
    Elem = null,
    decode_tuple(RestData, RestOids, Pool, Types, [Elem | Acc]);
decode_tuple(<<_Oid:?int32, Size:?int32, Data:Size/binary, RestData/binary>>,
             [Oid | RestOids], Pool, Types, Acc) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, Oid),
    Elem = Mod:decode(Data, TypeInfo),
    decode_tuple(RestData, RestOids, Pool, Types, [Elem | Acc]).

decode_tuple_no_oids(Data, Count, Pool, Types) ->
    decode_tuple_no_oids(Data, Count, Pool, Types, []).

decode_tuple_no_oids(<<>>, 0, _, _Types, Acc) ->
    list_to_tuple(lists:reverse(Acc));
decode_tuple_no_oids(<<_Oid:?int32, -1:?int32, RestData/binary>>,
                     Count, Pool, Types, Acc) ->
    Elem = null,
    decode_tuple_no_oids(RestData, Count-1, Pool, Types, [Elem | Acc]);
decode_tuple_no_oids(<<Oid:?int32, Size:?int32, Data:Size/binary, RestData/binary>>,
                     Count, Pool, Types, Acc) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, Oid),
    Elem = Mod:decode(Data, TypeInfo),
    decode_tuple_no_oids(RestData, Count-1, Pool, Types, [Elem | Acc]).
