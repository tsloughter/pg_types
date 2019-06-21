-module(pg_jsonb).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-define(JSONB_VERSION_1, 1).

init(_Opts) ->
    {[<<"jsonb_send">>], []}.

encode(Json, _) ->
    [<<?JSONB_VERSION_1:8>> | Json].

decode(<<?JSONB_VERSION_1:8, Bin/binary>>, _) ->
    Bin.
