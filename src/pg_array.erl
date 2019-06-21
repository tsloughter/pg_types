-module(pg_array).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_types.hrl").

init(_Opts) ->
    {[<<"array_send">>], []}.

encode(Array, #type_info{pool=Pool, elem_oid=Oid}) ->
    TypeInfo=#type_info{module=Mod} = pg_types:lookup_type_info(Pool, Oid),
    EncodedArrayElements = encode_array_elements(Array, Mod, TypeInfo, []),
    encode_array_binary(EncodedArrayElements, TypeInfo).

decode(Bin, TypeInfo) ->
    decode_array_bin(Bin, TypeInfo).

%%


encode_array_elements([{array, SubArray} | Tail], Mod, TypeInfo, Acc) ->
    SubArrayElements = encode_array_elements(SubArray, Mod, TypeInfo, []),
    encode_array_elements(Tail, Mod, TypeInfo, [{array, SubArrayElements} | Acc]);
encode_array_elements([null | Tail], Mod, TypeInfo, Acc) ->
    encode_array_elements(Tail, Mod, TypeInfo, [null | Acc]);
encode_array_elements([Element | Tail], Mod, TypeInfo, Acc) ->
    Encoded = Mod:encode(Element, TypeInfo),
    encode_array_elements(Tail, Mod, TypeInfo, [<<(byte_size(Encoded)):32/integer, Encoded/binary>> | Acc]);
encode_array_elements([], _Mod, _TypeInfo, Acc) ->
    lists:reverse(Acc).

encode_array_binary(ArrayElements, #type_info{oid=Oid}) ->
    {HasNulls, Rows} = encode_array_binary_row(ArrayElements, false, []),
    Dims = get_array_dims(ArrayElements),
    Header = encode_array_binary_header(Dims, HasNulls, Oid),
    [Header, Rows].

encode_array_binary_row([null | Tail], _HasNull, Acc) ->
    encode_array_binary_row(Tail, true, [<<-1:32/integer>> | Acc]);
encode_array_binary_row([<<_Size:32/integer, _Val/binary>> = Binary | Tail], HasNull, Acc) ->
    encode_array_binary_row(Tail, HasNull, [Binary | Acc]);
encode_array_binary_row([{array, Elements} | Tail], HasNull, Acc) ->
    {NewHasNull, Row} = encode_array_binary_row(Elements, HasNull, []),
    encode_array_binary_row(Tail, NewHasNull, [Row | Acc]);
encode_array_binary_row([], HasNull, AccRow) ->
    {HasNull, lists:reverse(AccRow)}.

get_array_dims([{array, SubElements} | _] = Row) ->
    Dims0 = get_array_dims(SubElements),
    Dim = length(Row),
    [Dim | Dims0];
get_array_dims(Row) ->
    Dim = length(Row),
    [Dim].

encode_array_binary_header(Dims, HasNulls, ElementTypeOID) ->
    NDims = length(Dims),
    Flags = if
        HasNulls -> 1;
        true -> 0
    end,
    EncodedDimensions = [<<Dim:32/integer, 1:32/integer>> || Dim <- Dims],
    [<<NDims:32/integer, Flags:32/integer, ElementTypeOID:32/integer>>, EncodedDimensions].

decode_array_bin(<<Dimensions:32/signed-integer, _Flags:32/signed-integer,
                   _ElementOID:32/signed-integer, Remaining/binary>>, TypeInfo) ->
    {RemainingData, DimsInfo} = lists:foldl(fun(_Pos, {Bin, Acc}) ->
                <<Nbr:32/signed-integer, LBound:32/signed-integer, Next/binary>> = Bin,
                {Next, [{Nbr, LBound} | Acc]}
        end, {Remaining, []}, lists:seq(1, Dimensions)),
    DataList = decode_array_bin_aux(RemainingData, TypeInfo, []),
    {array, Expanded} = expand(DataList, DimsInfo),
    Expanded.

expand([], []) ->
    {array, []};
expand([List], []) ->
    List;
expand(List, [{Nbr,_}|NextDim]) ->
    List2 = expand_aux(List, Nbr, Nbr, [], []),
    expand(List2, NextDim).

expand_aux([], 0, _, Current, Acc) ->
    lists:reverse([{array, lists:reverse(Current)} | Acc]);
expand_aux(List, 0, Nbr, Current, Acc) ->
    expand_aux(List, Nbr, Nbr, [], [ {array, lists:reverse(Current)} | Acc]);
expand_aux([E|Next], Level, Nbr, Current, Acc) ->
    expand_aux(Next, Level-1, Nbr, [E | Current], Acc).


decode_array_bin_aux(<<>>, _TypeInfo, Acc) ->
    lists:reverse(Acc);
decode_array_bin_aux(<<-1:32/signed-integer, Rest/binary>>, TypeInfo, Acc) ->
    decode_array_bin_aux(Rest, TypeInfo, [null | Acc]);
decode_array_bin_aux(<<Size:32/signed-integer, Next/binary>>, TypeInfo=#type_info{pool=Pool, elem_oid=ElemOid}, Acc) ->
    {ValueBin, Rest} = split_binary(Next, Size),
    ElemTypeInfo = pg_types:lookup_type_info(Pool, ElemOid),
    Value = pg_types:decode(ValueBin, ElemTypeInfo),
    decode_array_bin_aux(Rest, TypeInfo, [Value | Acc]).
