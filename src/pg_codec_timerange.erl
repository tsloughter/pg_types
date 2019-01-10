%%% @doc
%%% Codec for `tsrange', `tstzrange', `daterange' types.
%%% https://www.postgresql.org/docs/current/static/rangetypes.html#rangetypes-builtin
%%% $PG$/src/backend/utils/adt/rangetypes.c
%%% @end
%%% Created : 16 Jul 2018 by Vladimir Sekissov <eryx67@gmail.com>
%%% TODO: universal range, based on pg_range table
%%% TODO: inclusive/exclusive ranges `[]' `[)' `(]' `()'

-module(pg_codec_timerange).
-behaviour(pg_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-include("pg_datatypes.hrl").

-export_type([data/0]).

-type data() :: {pg_codec_datetime:data(), pg_codec_datetime:data()} | empty.

init(_, Sock) ->
    case pg_sock:get_parameter_internal(<<"integer_datetimes">>, Sock) of
        <<"on">>  -> pg_idatetime;
        <<"off">> -> pg_fdatetime
    end.

names() ->
    [tsrange, tstzrange, daterange].

encode(empty, _T, _CM) ->
    <<1>>;
encode({From, To}, Type, EncMod) ->
    FromBin = encode_member(Type, From, EncMod),
    ToBin = encode_member(Type, To, EncMod),
    <<2:1/big-signed-unit:8,
      (byte_size(FromBin)):?int32, FromBin/binary,
      (byte_size(ToBin)):?int32, ToBin/binary>>.

decode(<<1>>, _, _) ->
    empty;
decode(<<2:1/big-signed-unit:8,
         FromLen:?int32, FromBin:FromLen/binary,
         ToLen:?int32, ToBin:ToLen/binary>>,
       Type, EncMod) ->
    {decode_member(Type, FromBin, EncMod), decode_member(Type, ToBin, EncMod)}.

decode_text(V, _, _) -> V.

encode_member(Type, Val, pg_idatetime) ->
    pg_idatetime:encode(member_type(Type), Val);
encode_member(Type, Val, pg_fdatetime) ->
    pg_fdatetime:encode(member_type(Type), Val).

decode_member(Type, Bin, pg_idatetime) ->
    pg_idatetime:decode(member_type(Type), Bin);
decode_member(Type, Bin, pg_fdatetime) ->
    pg_fdatetime:decode(member_type(Type), Bin).

member_type(tsrange) -> timestamp;
member_type(tstzrange) -> timestamptz;
member_type(daterange) -> date.
