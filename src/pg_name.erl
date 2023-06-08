-module(pg_name).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"namesend">>], []}.

encode(Name, _) when byte_size(Name) < 64 ->
    [<<(iolist_size(Name)):?int32>>, Name].
decode(Name, _) ->
    Name.

type_spec()->
    "Name::io_list() when byte_size(Name) < 64".
