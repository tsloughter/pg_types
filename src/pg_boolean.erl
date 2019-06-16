-module(pg_boolean).

-include("pg_protocol.hrl").

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    [<<"boolsend">>].

encode(true, _) ->
    <<1:1/big-signed-unit:8>>;
encode(false, _) ->
    <<0:1/big-signed-unit:8>>.

decode(<<1:1/big-signed-unit:8>>, _) -> true;
decode(<<0:1/big-signed-unit:8>>, _) -> false.
