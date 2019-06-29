-module(pg_jsonb).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

-define(JSONB_VERSION_1, 1).

init(#{json_config := Config}) ->
    {[<<"json_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, json_config, []),
    {[<<"jsonb_send">>], Config}.

encode(Json, #type_info{config=[]}) ->
    [<<(iolist_size(Json) + 1):?int32, ?JSONB_VERSION_1:?int8>> | Json];
encode(Json, #type_info{config={Module, EncodeOptions, _}}) ->
    EncodedJson = Module:encode(Json, EncodeOptions),
    [<<(iolist_size(EncodedJson) + 1):?int32, ?JSONB_VERSION_1:?int8>> | EncodedJson].

decode(<<?JSONB_VERSION_1:?int8, Bin/binary>>, #type_info{config=[]}) ->
    Bin;
decode(<<?JSONB_VERSION_1:?int8, Bin/binary>>, #type_info{config={Module, _, DecodeOptions}}) ->
    Module:decode(Bin, DecodeOptions).
