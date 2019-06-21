-module(pg_json).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"json_send">>], []}.

encode(Json, _) ->
    Json.

decode(Bin, _) ->
    Bin.
