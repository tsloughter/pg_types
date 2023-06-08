-module(pg_json).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(#{json_config := Config}) ->
    {[<<"json_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, json_config, []),
    {[<<"json_send">>], Config}.

encode(Json, #type_info{config=[]}) ->
    [<<(iolist_size(Json)):?int32>>, Json];
encode(Json, #type_info{config={Module, EncodeOptions, _}}) ->
    EncodedJson = Module:encode(Json, EncodeOptions),
    [<<(iolist_size(EncodedJson)):?int32>>, EncodedJson].

decode(Bin, #type_info{config=[]}) ->
    Bin;
decode(Bin, #type_info{config={Module, _, DecodeOptions}}) ->
    Module:decode(Bin, DecodeOptions).

type_spec() ->
    "iolist() | list() | map()".
