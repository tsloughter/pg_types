-module(pg_point).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"point_send">>], []}.

encode(#{x := X, y := Y}, _) ->
    <<16:?int32, X:?float64, Y:?float64>>.

decode(<<X:64/float, Y:64/float>>, _) ->
    #{x => X, y => Y}.
