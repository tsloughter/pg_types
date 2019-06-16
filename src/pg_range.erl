-module(pg_range).

-export([typsend/0, encode/2, decode/2]).

-include("pg_datatypes.hrl").

-export_type([data/0]).

-type data() :: {left(), right()}.

-type left() :: minus_infinity | integer().
-type right() :: plus_infinity | integer().


typsend() ->
    [<<"range_send">>].

encode({From, To}, #type_info{pool=Pool, base_oid=BaseOid}) ->
    {Mod, TypeInfo} = pg_datatypes:lookup_type_module(Pool, BaseOid),
    EncodedFrom = Mod:encode(From, TypeInfo),
    EncodedTo = Mod:encode(To, TypeInfo),
    [<<2:1/big-signed-unit:8>>, <<(iolist_size(EncodedFrom)):32>>, EncodedFrom, <<(iolist_size(EncodedTo)):32>>, EncodedTo].

decode(<<1>>, _) ->
    1;
decode(<<2:8, Rest/binary>>, #type_info{pool=Pool, base_oid=BaseOid}) ->
    {Mod, BaseTypeInfo=#type_info{typlen=Len}} = pg_datatypes:lookup_type_module(Pool, BaseOid),
    <<Len:?int32, From:Len/binary, Len:?int32, To:Len/binary>> = Rest,
    DecodedFrom = Mod:decode(From, BaseTypeInfo),
    DecodedTo = Mod:decode(To, BaseTypeInfo),
    {DecodedFrom, DecodedTo}.

%% encode(Range, int4range, _) ->
%%     encode_int4range(Range);
%% encode(Range, int8range, _) ->
%%     encode_int8range(Range).

%% decode(Bin, int4range, _) ->
%%     decode_int4range(Bin);
%% decode(Bin, int8range, _) ->
%%     decode_int8range(Bin).


%% encode_int4range({minus_infinity, plus_infinity}) ->
%%     <<24:1/big-signed-unit:8>>;
%% encode_int4range({From, plus_infinity}) ->
%%     FromInt = to_int(From),
%%     <<18:1/big-signed-unit:8, 4:?int32, FromInt:?int32>>;
%% encode_int4range({minus_infinity, To}) ->
%%     ToInt = to_int(To),
%%     <<8:1/big-signed-unit:8, 4:?int32, ToInt:?int32>>;
%% encode_int4range({From, To}) ->
%%     FromInt = to_int(From),
%%     ToInt = to_int(To),
%%     <<2:1/big-signed-unit:8, 4:?int32, FromInt:?int32, 4:?int32, ToInt:?int32>>.

%% encode_int8range({minus_infinity, plus_infinity}) ->
%%     <<24:1/big-signed-unit:8>>;
%% encode_int8range({From, plus_infinity}) ->
%%     FromInt = to_int(From),
%%     <<18:1/big-signed-unit:8, 8:?int32, FromInt:?int64>>;
%% encode_int8range({minus_infinity, To}) ->
%%     ToInt = to_int(To),
%%     <<8:1/big-signed-unit:8, 8:?int32, ToInt:?int64>>;
%% encode_int8range({From, To}) ->
%%     FromInt = to_int(From),
%%     ToInt = to_int(To),
%%     <<2:1/big-signed-unit:8, 8:?int32, FromInt:?int64, 8:?int32, ToInt:?int64>>.

%% to_int(N) when is_integer(N) -> N;
%% to_int(S) when is_list(S) -> erlang:list_to_integer(S);
%% to_int(B) when is_binary(B) -> erlang:binary_to_integer(B).


%% decode_int4range(<<2:1/big-signed-unit:8, 4:?int32, From:?int32, 4:?int32, To:?int32>>) ->
%%     {From, To};
%% decode_int4range(<<8:1/big-signed-unit:8, 4:?int32, To:?int32>>) ->
%%     {minus_infinity, To};
%% decode_int4range(<<18:1/big-signed-unit:8, 4:?int32, From:?int32>>) ->
%%     {From, plus_infinity};
%% decode_int4range(<<24:1/big-signed-unit:8>>) ->
%%     {minus_infinity, plus_infinity}.

%% decode_int8range(<<2:1/big-signed-unit:8, 8:?int32, From:?int64, 8:?int32, To:?int64>>) ->
%%     {From, To};
%% decode_int8range(<<8:1/big-signed-unit:8, 8:?int32, To:?int64>>) ->
%%     {minus_infinity, To};
%% decode_intu8range(<<18:1/big-signed-unit:8, 8:?int32, From:?int64>>) ->
%%     {From, plus_infinity};
%% decode_int8range(<<24:1/big-signed-unit:8>>) ->
%%     {minus_infinity, plus_infinity}.

%% decode_text(V, _, _) -> V.
