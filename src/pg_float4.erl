-module(pg_float4).

-export([typsend/0,
         encode/2,
         decode/2]).

-define(POS_INF,  <<0:1, 255:8, 0:23>>).
-define(NEG_INF,  <<1:1, 255:8, 0:23>>).
-define(NAN_PATTERN, <<_:1, 255:8, _:23>>).
-define(NAN, <<0:1, 255:8, 1:1, 0:22>>).

typsend() ->
    <<"float4send">>.

encode(Int, _) when is_integer(Int) ->
    <<(Int * 1.0):1/big-float-unit:32>>;
encode(Float, _) when is_float(Float) ->
    <<Float:1/big-float-unit:32>>;
encode(nan, _) ->
    ?NAN;
encode(plus_infinity, _) ->
    ?POS_INF;
encode(minus_infinity, _) ->
    ?NEG_INF.

decode(<<Float:1/big-float-unit:32>>, _) ->
    Float;
decode(?POS_INF,  _) ->
    plus_infinity;
decode(?NEG_INF, _) ->
    minus_infinity;
decode(?NAN_PATTERN, _) ->
    nan.
