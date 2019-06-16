-module(pg_macaddr).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    [<<"macaddr_send">>].

encode({B1, B2, B3, B4, B5, B6}, _) ->
    <<B1, B2, B3, B4, B5, B6>>.

decode(<<B1, B2, B3, B4, B5, B6>>, _) ->
    {B1, B2, B3, B4, B5, B6}.
