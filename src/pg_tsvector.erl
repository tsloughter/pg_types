-module(pg_tsvector).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"tsvectorsend">>], []}.

encode(Val, _) when is_list(Val) ->
    Encoded = encode_tsvector(Val),
    [<<(iolist_size(Encoded)):?int32>>, Encoded].

decode(<<_Lexemes:?int32, Words/binary>>, _) ->
    decode_tsvector_values(Words).

%%

encode_tsvector(Values) ->
    [<<(length(Values)):?int32>>, encode_lexemes(Values)].

encode_lexemes(Values) ->
    [encode_positions(V) || V <- Values].

encode_positions({Word, Positions}) ->
    EncodedPositions = [<<(encode_weight(Weight)):2, Position:14>> || {Position, Weight} <- Positions],
    [Word, 0, <<(length(Positions)):16>>, EncodedPositions].

decode_tsvector_values(<<>>) ->
    [];
decode_tsvector_values(Words) ->
    [Word, <<PosCount:16, Rest/binary>>] = binary:split(Words, <<0>>),
    PosBytes = PosCount * 2,
    <<Positions:PosBytes/binary, Remaining/binary>> = Rest,
    DecodedPositions = [{Position, decode_weight(Weight)} || <<Weight:2, Position:14>> <= Positions],
    [{Word, DecodedPositions} | decode_tsvector_values(Remaining)].

encode_weight('A') ->
    3;
encode_weight('B') ->
    2;
encode_weight('C') ->
    1;
encode_weight(null) ->
    0.

decode_weight(0) ->
    null;
decode_weight(1) ->
    'C';
decode_weight(2) ->
    'B';
decode_weight(3) ->
    'A'.
