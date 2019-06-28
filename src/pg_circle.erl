-module(pg_circle).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"circle_send">>], []}.

encode(#{center := #{x := X,
                     y := Y},
         radius := R}, _) ->
    <<24:?int32, X:?float64, Y:?float64, R:?float64>>.

decode(<<X:?float64, Y:?float64, R:?float64>>, _) ->
    #{center => #{x => X,
                  y => Y},
      radius => R}.
