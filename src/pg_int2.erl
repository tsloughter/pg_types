-module(pg_int2).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    <<"int2send">>.

encode(N,  _) ->
    <<N:1/big-signed-unit:16>>.

decode(<<N:1/big-signed-unit:16>>, _) ->
    N.
