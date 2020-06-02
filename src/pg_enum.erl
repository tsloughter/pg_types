-module(pg_enum).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(#{enum_config := Config}) ->
    {[<<"enum_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, enum_config, []),
    {[<<"enum_send">>], Config}.

encode(Atom, #type_info{config = atoms} = Args) when is_atom(Atom) ->
    encode(atom_to_binary(Atom, utf8), Args);
encode(Text, _) ->
    [<<(iolist_size(Text)):?int32>>, Text].

decode(Text, #type_info{config = existing_atoms}) ->
    binary_to_existing_atom(Text, utf8);
decode(Text, #type_info{config = atoms}) ->
    binary_to_atom(Text, utf8);
decode(Text, _) ->
    Text.

type_spec() ->
    "atom() | binary()".
