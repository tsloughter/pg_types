-module(pg_timestampz).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"timestamptz_send">>], []}.

encode(Timestamp, _TypeInfo) ->
    <<(pg_timestamp:encode_timestamp(Timestamp)):64>>.

decode(Bin, _TypeInfo) ->
    pg_timestamp:decode_timestamp(Bin).
