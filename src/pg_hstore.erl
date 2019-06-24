-module(pg_hstore).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         format_error/1]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"hstore_send">>], []}.

encode(Map, _) when is_map(Map) ->
    Data = encode_hstore(Map),
    [<<(iolist_size(Data)):?int32>> | Data];
encode(M, _) ->
    erlang:error(?badarg({not_a_map, M})).

decode(<<Size:?int32, Elements/binary>>, _) ->
    decode_hstore(Size, Elements, #{}).

format_error({not_a_map, M}) ->
    io_lib:format("hstore argument must be a map. instead got: ~p", [M]).


%%

encode_hstore(Map) ->
    Encoded = maps:fold(fun(K, V, Acc) ->
                            [Acc, encode_key(K), encode_value(V)]
                        end, [], Map),

    [<<(maps:size(Map)):?int32>>, Encoded].

encode_key(Key) ->
    KeyStr = to_binary(Key),
    [<<(iolist_size(KeyStr)):?int32>>, KeyStr].

encode_value(Value) ->
    [<<(iolist_size(Value)):?int32>>, Value].

to_binary(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
to_binary(Key) when is_list(Key) ->
    list_to_binary(Key);
to_binary(Key) when is_binary(Key) ->
    Key.

decode_hstore(0, <<>>, Map) ->
    Map;
decode_hstore(N, <<KeyLen:?int32, Key:KeyLen/binary, -1:?int32, Rest/binary>>, Map) ->
    decode_hstore(N - 1, Rest, Map#{Key => null});
decode_hstore(N, <<KeyLen:?int32, Key:KeyLen/binary,
                   ValLen:?int32, Value:ValLen/binary, Rest/binary>>, Map) ->
    decode_hstore(N - 1, Rest, Map#{Key => Value}).
