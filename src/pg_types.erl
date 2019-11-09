-module(pg_types).

-export([encode/2,
         decode/2,
         encode/3,
         decode/3,
         format_error/1,
         lookup_type_info/2,
         update/3,
         update_map/3]).

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

-spec update(atom(), [type_info()], map()) -> ok.
update(Pool, TypeInfos, Parameters) ->
    Types = update_map(TypeInfos, Parameters, #{}),
    maps:map(fun(Oid, TypeInfo) ->
                     persistent_term:put({?MODULE, Pool, Oid}, TypeInfo)
             end, Types),
    ok.

-spec update_map([type_info()], map(), map()) -> map().
update_map(TypeInfos, Parameters, ToUpdate) ->
    {ok, Modules} = application:get_key(pg_types, modules),
    Modules1 = Modules ++ application:get_env(pg_types, modules, []),
    Result = lists:foldl(fun(Module, Acc) ->
                                 add_type(Module, TypeInfos, Parameters, Acc)
                         end, ToUpdate, Modules1),
    maps:map(fun(_K, TypeInfo) ->
                     maybe_update_subtypes(Result, TypeInfo)
             end, Result).

add_type(Module, TypeInfos, Parameters, ToUpdate) ->
    try Module:init(Parameters) of
        {TypeSends, Config} ->
            update_for_typesends(Module, Config, TypeSends, TypeInfos, ToUpdate)
    catch
        _:_ ->
            %% expected for modules that aren't for encode/decode
            ToUpdate
    end.

lookup_typsends(TypeInfos, TypeSend) ->
    lists:filter(fun(T) -> T#type_info.typsend =:= TypeSend end, TypeInfos).

lookup_type_info(Pool, Oid) ->
    case persistent_term:get({?MODULE, Pool, Oid}, undefined) of
        undefined ->
            unknown_oid;
        TypeInfo ->
            TypeInfo
    end.

update_for_typesends(Module, Config, TypeSends, TypeInfos, Map) ->
    lists:foldl(fun(TypeSend, Acc) ->
                    lists:foldl(fun(TypeInfo=#type_info{oid=Oid}, Acc1) ->
                                        Acc1#{Oid => TypeInfo#type_info{module=Module,
                                                                        config=Config}}
                                end, Acc, lookup_typsends(TypeInfos, TypeSend))
                end, Map, TypeSends).

maybe_update_subtypes(_Map, unknown_oid) ->
    unknown_oid;
maybe_update_subtypes(Map, TypeInfo) ->
    TypeInfo1 = maybe_add_elem_type(Map, TypeInfo),
    maybe_add_comp_types(Map, TypeInfo1).

maybe_add_elem_type(_, T=#type_info{elem_oid=0}) ->
    T;
maybe_add_elem_type(Map, T=#type_info{elem_oid=ElemOid}) ->
    T#type_info{elem_type=maybe_update_subtypes(Map, maps:get(ElemOid, Map, unknown_oid))}.

maybe_add_comp_types(_, T=#type_info{comp_oids=[]}) ->
    T;
maybe_add_comp_types(Map, T=#type_info{comp_oids=CompOids}) ->
    CompTypes = [maybe_update_subtypes(Map, maps:get(CompOid, Map, unknown_oid)) || CompOid <- CompOids],
    T#type_info{comp_types=CompTypes}.
