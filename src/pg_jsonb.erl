-module(pg_jsonb).

-export([typsend/0,
         encode/2,
         decode/2]).

-define(JSONB_VERSION_1, 1).

typsend() ->
    <<"jsonb_send">>.

encode(Json, _) ->
    [<<?JSONB_VERSION_1:8>> | Json].

decode(<<?JSONB_VERSION_1:8, Bin/binary>>, _) ->
    Bin.
