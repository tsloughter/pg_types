-module(pg_types).

-export([encode/2,
         decode/2,
         encode/3,
         decode/3,
         format_error/1,
         lookup_type_info/2,
         update/3]).

-include_lib("pg_types.hrl").

-type oid() :: integer().
-type error() :: {badarg, {module(), term()}}.
-type parameters() :: #{binary() => binary()}.

-export_type([oid/0,
              error/0,
              parameters/0]).

-type typsend() :: binary().
-type type_info() :: #type_info{}.
-type opts() :: term().

-callback init(map()) -> {[typsend()], opts()}.

%% encode must return the size at the beginning of the iodata
-callback encode(term(), type_info()) -> iodata().
-callback decode(binary(), type_info()) -> term().

-callback format_error(term()) -> string().

-optional_callbacks([format_error/1]).

-ignore_xref([{decode, 3}, {behaviour_info, 1},
              {encode, 3}, {encode, 2}, {update, 3}]).

-spec encode(term(), type_info()) -> iodata().
encode(Value, TypeInfo=#type_info{module=Module}) ->
    Module:encode(Value, TypeInfo).

-spec decode(binary(), type_info()) -> term().
decode(Value, TypeInfo=#type_info{module=Module}) ->
    Module:decode(Value, TypeInfo).

-spec encode(atom(), any(), oid()) -> iodata().
encode(Pool, Value, Oid) ->
    encode(Value, lookup_type_info(Pool, Oid)).

-spec decode(atom(), binary(), oid()) -> term().
decode(Pool, Value, Oid) ->
    decode(Value, lookup_type_info(Pool, Oid)).

-spec format_error(error()) -> string().
format_error({badarg, {Module, Reason}}) ->
    Module:format_error(Reason).

update(Pool, TypeInfos, Parameters) ->
    {ok, Modules} = application:get_key(pg_types, modules),
    Modules1 = Modules ++ application:get_env(pg_types, modules, []),
    [add_type(Pool, Module, TypeInfos, Parameters) || Module <- Modules1].

add_type(Pool, Module, TypeInfos, Parameters) ->
    try Module:init(Parameters) of
        {TypeSends, Config} ->
            [[persistent_term:put({?MODULE, Pool, Oid}, TypeInfo#type_info{module=Module,
                                                                           config=Config})
              || TypeInfo=#type_info{oid=Oid} <- lookup_typsend(TypeInfos, TypeSend)]
             || TypeSend <- TypeSends]
    catch
        _:_ ->
            %% expected for modules that aren't for encode/decode
            ok
    end.

lookup_typsend(TypeInfos, TypeSend) ->
    lists:filter(fun(T) -> T#type_info.typsend =:= TypeSend end, TypeInfos).

lookup_type_info(Pool, Oid) ->
    case persistent_term:get({?MODULE, Pool, Oid}, undefined) of
        undefined ->
            unknown_oid;
        TypeInfo ->
            TypeInfo
    end.

