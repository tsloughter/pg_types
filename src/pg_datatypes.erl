-module(pg_datatypes).

-export([encode/3,
         decode/3,
         lookup_type_module/2,
         update/3]).

-type oid() :: integer().
-include_lib("pg_datatypes/include/pg_datatypes.hrl").

-export_type([parameters/0]).

-type parameters() :: #{binary() => binary()}.

-callback encode(term(), oid(), parameters()) -> iodata().
-callback decode(binary(), oid(), parameters()) -> term().

-spec encode(atom(), any(), oid()) -> iodata().
encode(Pool, Value, Oid) ->
    {Mod, TypeInfo} = lookup_type_module(Pool, Oid),
    Mod:encode(Value, TypeInfo).

-spec decode(atom(), binary(), oid()) -> term().
decode(Pool, Value, Oid) ->
    {Mod, TypeInfo} = lookup_type_module(Pool, Oid),
    Mod:decode(Value, TypeInfo).

update(Pool, Oids, _Parameters) ->
    {ok, Modules} = application:get_key(pg_datatypes, modules),
    Modules1 = Modules ++ application:get_env(pg_datatypes, modules, []),
    [add_type(Pool, Module, Oids) || Module <- Modules1].

add_type(Pool, Module, Oids) ->
    try Module:typsend() of
        TypeSends when is_list(TypeSends) ->
            [[persistent_term:put({?MODULE, Pool, Oid}, {Module, TypeInfo})
              || TypeInfo=#type_info{oid=Oid} <- lookup_typsend(Oids, TypeSend)]
             || TypeSend <- TypeSends];

        TypeSend ->
            [persistent_term:put({?MODULE, Pool, Oid}, {Module, TypeInfo})
             || TypeInfo=#type_info{oid=Oid} <- lookup_typsend(Oids, TypeSend)]
    catch
        _:_ ->
            %% expected for modules that aren't for encode/decode
            ok
    end.

lookup_typsend(Oids, TypeSend) ->
    lists:filter(fun(T) -> T#type_info.typsend =:= TypeSend end, Oids).

lookup_type_module(Pool, Oid) ->
    case persistent_term:get({?MODULE, Pool, Oid}, undefined) of
        undefined ->
            unknown_oid;
        Mod ->
            Mod
    end.

