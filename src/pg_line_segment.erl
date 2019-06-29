-module(pg_line_segment).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-dialyzer(no_improper_lists).

init(_Opts) ->
    {[<<"lseg_send">>], []}.

encode(#{point1 := #{x := X1, y := Y1}, point2 := #{x := X2, y := Y2}}, _) ->
    EncodedP1 = <<X1:?float64, Y1:?float64>>,
    EncodedP2 = <<X2:?float64, Y2:?float64>>,
    [<<32:?int32>>, EncodedP1 | EncodedP2].

decode(<<X1:?float64, Y1:?float64, X2:?float64, Y2:?float64>>, _) ->
    P1 = #{x => X1, y => Y1},
    P2 = #{x => X2, y => Y2},
    #{point1 => P1, point2 => P2}.
