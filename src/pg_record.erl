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
                         comp_types=CompTypes}) ->
    Data = encode_tuple(tuple_to_list(Tuple), CompTypes, Pool),
    [<<(iolist_size(Data) + 4):?int32, (erlang:tuple_size(Tuple)):?int32>> | Data].

decode(<<Count:?int32, Data/binary>>, #type_info{pool=Pool,
                                                 comp_types=[]}) ->
    decode_tuple_no_oids(Data, Count, Pool, []);
decode(<<Count:?int32, Data/binary>>, #type_info{pool=Pool,
                                                 comp_types=undefined}) ->
    decode_tuple_no_oids(Data, Count, Pool, []);
decode(<<_:?int32, Data/binary>>, #type_info{pool=Pool,
                                             comp_types=CompTypes}) ->
    Types = [],
    decode_tuple(Data, CompTypes, Pool, Types).
%%

encode_tuple(Elems, CompTypes, Pool) ->
    encode_tuple(Elems, CompTypes, Pool, []).

encode_tuple([], [], _, Acc) ->
    Acc;
encode_tuple([null | T], [#type_info{oid=Oid} | CompTypes], Pool, Acc) ->
    EncodedElem = <<-1:?int32>>,
    encode_tuple(T, CompTypes, Pool, Acc ++ [<<Oid:?int32>>, EncodedElem]);
encode_tuple([H | T], [TypeInfo=#type_info{oid=Oid,
                                           module=Mod} | CompTypes], Pool, Acc) ->
    EncodedElem = Mod:encode(H, TypeInfo),
    encode_tuple(T, CompTypes, Pool, Acc ++ [<<Oid:?int32>>, EncodedElem]).

decode_tuple(Data, CompTypes, Pool, Types) ->
    decode_tuple(Data, CompTypes, Pool, Types, []).

decode_tuple(<<>>, [], _, _Types, Acc) ->
    list_to_tuple(lists:reverse(Acc));
decode_tuple(<<_Oid:?int32, -1:?int32, RestData/binary>>,
             [_TypeInfo | CompTypes], Pool, Types, Acc) ->
    Elem = null,
    decode_tuple(RestData, CompTypes, Pool, Types, [Elem | Acc]);
decode_tuple(<<_Oid:?int32, Size:?int32, Data:Size/binary, RestData/binary>>,
             [TypeInfo=#type_info{module=Mod} | CompTypes], Pool, Types, Acc) ->
    Elem = Mod:decode(Data, TypeInfo),
    decode_tuple(RestData, CompTypes, Pool, Types, [Elem | Acc]).

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
