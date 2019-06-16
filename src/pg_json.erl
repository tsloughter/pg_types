-module(pg_json).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    <<"json_send">>.

encode(Json, _) ->
    Json.

decode(Bin, _) ->
    Bin.
