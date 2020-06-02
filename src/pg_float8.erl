-module(pg_float8).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

-define(POS_INF8, <<0:1, 2047:11, 0:52>>).
-define(NEG_INF8, <<1:1, 2047:11, 0:52>>).
-define(NAN_PATTERN8, <<_:1, 2047:11, _:52>>).
-define(NAN8, <<0:1, 2047:11, 1:1, 0:51>>).

init(_Opts) ->
    {[<<"float8send">>], []}.

encode(Int, _) when is_integer(Int) ->
    <<8:?int32, (Int * 1.0):?float64>>;
encode(Float, _) when is_float(Float) ->
    <<8:?int32, Float:?float64>>;
encode(nan, _) ->
    [<<8:?int32>>, ?NAN8];
encode(plus_infinity, _) ->
    [<<8:?int32>>, ?POS_INF8];
encode(minus_infinity, _) ->
    [<<8:?int32>>, ?NEG_INF8].

decode(<<Float:?float64>>, _) ->
    Float;
decode(?POS_INF8, _) ->
    plus_infinity;
decode(?NEG_INF8, _) ->
    minus_infinity;
decode(?NAN_PATTERN8, _) ->
    nan.

type_spec() ->
    "float()".
