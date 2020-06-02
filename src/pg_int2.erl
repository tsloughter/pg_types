-module(pg_int2).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"int2send">>], []}.

encode(N,  _) when is_integer(N) ->
    <<2:?int32, N:?int16>>.

decode(<<N:?int16>>, _) ->
    N.

type_spec() ->
    "integer()".
