-module(pg_int4).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"int4send">>], []}.

encode(N,  _) ->
    <<N:1/big-signed-unit:32>>.

decode(<<N:1/big-signed-unit:32>>, _) ->
    N.
