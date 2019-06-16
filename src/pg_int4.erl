-module(pg_int4).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    <<"int4send">>.

encode(N,  _) ->
    <<N:1/big-signed-unit:32>>.

decode(<<N:1/big-signed-unit:32>>, _) ->
    N.
