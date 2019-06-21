-module(pg_macaddr).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"macaddr_send">>], []}.

encode({B1, B2, B3, B4, B5, B6}, _) ->
    <<6:?int32, B1, B2, B3, B4, B5, B6>>.

decode(<<B1, B2, B3, B4, B5, B6>>, _) ->
    {B1, B2, B3, B4, B5, B6}.
