-module(pg_tid).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"tidsend">>], []}.

encode({Block, Tuple}, _) ->
    <<6:?int32, Block:?uint32, Tuple:?uint16>>.

decode(<<Block:?uint32, Tuple:?uint16>>, _) ->
    {Block, Tuple}.
