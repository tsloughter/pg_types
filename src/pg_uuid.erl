-module(pg_uuid).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

-type format() :: binary | string | integer.
-export_type([format/0]).

init(#{uuid_format := Config}) when Config =:= binary ;
                                    Config =:= string ;
                                    Config =:= integer ->
    {[<<"uuid_send">>], Config};
init(_Opts) ->
    Config = application:get_env(pg_types, uuid_format, binary),
    {[<<"uuid_send">>], Config}.

encode(<<>>, _) ->
    <<-1:32/integer>>;
encode(<<Uuid:?int128>>, _) ->
    <<16:?int32, Uuid:?int128>>;
encode(Uuid, _) when is_integer(Uuid) ->
    <<16:?int32, Uuid:?int128>>;
encode(Uuid, _) when is_list(Uuid) ->
    Hex = [H || H <- Uuid, H =/= $-],
    {ok, [Int], _} = io_lib:fread("~16u", Hex),
    <<16:?int32, Int:?int128>>;
encode(Uuid, _) when is_binary(Uuid) ->
    Hex = binary:replace(Uuid, <<"-">>, <<>>, [global]),
    Int = erlang:binary_to_integer(Hex, 16),
    <<16:?int32, Int:?int128>>.

decode(Uuid, #type_info{config=binary}) ->
    Uuid;
decode(<<U0:32, U1:16, U2:16, U3:16, U4:48>>, #type_info{config=string}) ->
    Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
    iolist_to_binary(io_lib:format(Format, [U0, U1, U2, U3, U4]));
decode(<<Uuid:?int128>>, #type_info{config=integer}) ->
    Uuid.

type_spec() ->
    "<<>> | <<_:128>> | integer() | string() | binary()".
