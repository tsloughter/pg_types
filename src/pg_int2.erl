-module(pg_int2).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"int2send">>], []}.

encode(N,  _) ->
    <<N:1/big-signed-unit:16>>.

decode(<<N:1/big-signed-unit:16>>, _) ->
    N.
