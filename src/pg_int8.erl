-module(pg_int8).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    <<"int8send">>.

encode(N,  _) ->
    <<N:1/big-signed-unit:64>>.

decode(<<N:1/big-signed-unit:64>>, _) ->
    N.
