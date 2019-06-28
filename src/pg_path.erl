-module(pg_path).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"path_send">>], []}.

encode(#{open := O, points := Points}, _) ->
    OpenByte = case O of true -> 0; false -> 1 end,
    Len = length(Points),
    EncodedPoints = lists:foldl(fun(#{x := X, y := Y}, Acc) ->
                                        [Acc | [<<X:?float64, Y:?float64>>]]
                                end, [], Points),
    Nbytes = 5 + 16 * Len,
    [<<Nbytes:?int32>>, OpenByte, <<Len:?int32>> | EncodedPoints].

decode(<<OpenByte, Len:?int32, EncodedPoints/binary>>, _) ->
    Open = case OpenByte of 0 -> true; 1 -> false end,
    Points = decode_points(EncodedPoints, Len),
    #{open => Open, points => Points}.

decode_points(<<>>, 0) ->
    [];
decode_points(<<X:64/float, Y:64/float, Rest/binary>>, Count) ->
    [#{x => X, y => Y} | decode_points(Rest, Count-1)].
