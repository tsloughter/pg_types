-module(pg_jsonb).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-define(JSONB_VERSION_1, 1).

init(_Opts) ->
    {[<<"jsonb_send">>], []}.

encode(Json, _) ->
    [<<(iolist_size(Json) + 1):?int32, ?JSONB_VERSION_1:?int8>> | Json].

decode(<<?JSONB_VERSION_1:?int8, Bin/binary>>, _) ->
    Bin.
