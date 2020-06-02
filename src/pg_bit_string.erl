-module(pg_bit_string).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

-dialyzer(no_improper_lists).

init(_Opts) ->
    {[<<"bit_send">>, <<"varbit_send">>], []}.

encode(Val, _) when is_binary(Val) ->
    [<<(byte_size(Val) + 4):?int32, (bit_size(Val)):?int32>> | Val];
encode(Val, _) when is_bitstring(Val) ->
    BinSize = byte_size(Val),
    LastPos = BinSize - 1,
    <<Binary:LastPos/binary, Last/bits>> = Val,
    Pad = 8 - bit_size(Last),
    BitCount = bit_size(Val),
    [<<(BinSize + 4):?int32, BitCount:?int32>>, Binary | <<Last/bits, 0:Pad>>].

decode(<<Len:?int32, Bits:Len/bits, _/bits>>, _) ->
    Bits.

type_spec() ->
    "bitstring()".
