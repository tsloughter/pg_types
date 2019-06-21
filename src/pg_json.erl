-module(pg_json).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"json_send">>], []}.

encode(Json, _) ->
    [<<(iolist_size(Json)):?int32>>, Json].

decode(Bin, _) ->
    Bin.
