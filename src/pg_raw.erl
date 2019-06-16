-module(pg_raw).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    [<<"bpcharsend">>, <<"textsend">>,
     <<"varcharsend">>, <<"charsend">>,
     <<"byteasend">>,  <<"enum_send">>,
     <<"unknownsend">>, <<"citextsend">>].

encode(Text, _) ->
    Text.

decode(Text, _) ->
    Text.
