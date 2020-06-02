-module(pg_boolean).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"boolsend">>], []}.

encode(true, _) ->
    <<1:?int32, 1:?int8>>;
encode(false, _) ->
    <<1:?int32, 0:?int8>>.

decode(<<1:?int8>>, _) -> true;
decode(<<0:?int8>>, _) -> false.

type_spec() ->
    "true | false".
