-module(pg_polygon).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"poly_send">>], []}.

encode(#{vertices := Vertices}, _) ->
    Len = length(Vertices),
    EncodedPoints = lists:foldl(fun(#{x := X, y := Y}, Acc) ->
                                        [Acc | [<<X:?float64, Y:?float64>>]]
                                end, [], Vertices),
    Nbytes = 4 + 16 * Len,
    [<<Nbytes:?int32, Len:?int32>> | EncodedPoints].

decode(<<Len:?int32, EncodedPoints/binary>>, _) ->
    Points = decode_points(EncodedPoints, Len),
    #{vertices => Points}.

decode_points(<<>>, 0) ->
    [];
decode_points(<<X:64/float, Y:64/float, Rest/binary>>, Count) ->
    [#{x => X, y => Y} | decode_points(Rest, Count-1)].
