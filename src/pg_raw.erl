-module(pg_raw).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"bpcharsend">>, <<"textsend">>,
      <<"varcharsend">>, <<"charsend">>,
      <<"byteasend">>, <<"unknownsend">>,
      <<"citextsend">>], []}.

encode(Text, _) ->
    [<<(iolist_size(Text)):?int32>>, Text].

decode(Text, _) ->
    Text.

type_spec() ->
    "iolist()".
