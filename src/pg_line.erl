-module(pg_line).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"line_send">>], []}.

encode(#{a := A, b := B, c := C}, _) ->
    <<24:?int32, A:?float64, B:?float64, C:?float64>>.

decode(<<A:?float64, B:?float64, C:?float64>>, _) ->
    #{a => A, b => B, c => C}.

type_spec() ->
    "#{a := A::number(), b := B::number(), c := C::number()}".
