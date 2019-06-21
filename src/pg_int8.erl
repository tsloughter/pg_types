-module(pg_int8).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"int8send">>], []}.

encode(N, _) ->
    <<N:1/big-signed-unit:64>>.

decode(<<N:1/big-signed-unit:64>>, _) ->
    N.
