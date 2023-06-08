-module(pg_point).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"point_send">>], []}.

encode(#{x := X, y := Y}, _)  when is_number(X) andalso is_number(Y) ->
    <<16:?int32, X:?float64, Y:?float64>>.

decode(<<X:64/float, Y:64/float>>, _) ->
    #{x => X, y => Y}.

type_spec() ->
    "#{x := X::integer(), y := Y::integer()}".
