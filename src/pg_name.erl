-module(pg_name).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

init(_Opts) ->
    {[<<"namesend">>], []}.

encode(Name, _) when byte_size(Name) < 64 ->
    [<<(iolist_size(Name)):?int32>>, Name];
encode(_Name, _) ->
    erlang:error({badarg, name_too_large}).

decode(Name, _) ->
    Name.
