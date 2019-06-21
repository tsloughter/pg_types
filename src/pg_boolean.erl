-module(pg_boolean).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"boolsend">>], []}.

encode(true, _) ->
    <<1:1/big-signed-unit:8>>;
encode(false, _) ->
    <<0:1/big-signed-unit:8>>.

decode(<<1:1/big-signed-unit:8>>, _) -> true;
decode(<<0:1/big-signed-unit:8>>, _) -> false.
