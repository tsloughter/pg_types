-module(pg_timestampz).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    <<"timestamptz_send">>.

encode(Timestamp, _TypeInfo) ->
    <<(pg_timestamp:encode_timestamp(Timestamp)):64>>.

decode(Bin, _TypeInfo) ->
    pg_timestamp:decode_timestamp(Bin).
