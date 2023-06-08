-module(pg_int4).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"int4send">>], []}.

encode(N,  _) when is_integer(N) ->
    <<4:?int32, N:?int32>>.

decode(<<N:?int32>>, _) ->
    N.

type_spec() ->
    "integer()".
