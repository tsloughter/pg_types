-module(pg_uuid).

-export([typsend/0,
         encode/2,
         decode/2]).

typsend() ->
    [<<"uuid_send">>].

encode(<<Uuid:128>>, _) ->
    <<Uuid:128/big-unsigned-integer>>;
encode(Uuid, _) when is_integer(Uuid) ->
    <<Uuid:128/big-unsigned-integer>>;
encode(Uuid, _) when is_list(Uuid) ->
    Hex = [H || H <- Uuid, H =/= $-],
    {ok, [Int], _} = io_lib:fread("~16u", Hex),
    <<Int:128/big-unsigned-integer>>;
encode(Uuid, _) when is_binary(Uuid) ->
    Hex = binary:replace(Uuid, <<"-">>, <<>>, [global]),
    Int = erlang:binary_to_integer(Hex, 16),
    <<Int:128/big-unsigned-integer>>.

decode(<<U0:32, U1:16, U2:16, U3:16, U4:48>>, _) ->
    Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
    iolist_to_binary(io_lib:format(Format, [U0, U1, U2, U3, U4])).


%% decode_value_bin(?UUIDOID, Value, _OIDMap, _DecodeOptions) ->
%%     <<UUID_A:32/integer, UUID_B:16/integer, UUID_C:16/integer, UUID_D:16/integer, UUID_E:48/integer>> = Value,
%%     UUIDStr = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [UUID_A, UUID_B, UUID_C, UUID_D, UUID_E]),
%%     list_to_binary(UUIDStr);
%%     %% Value;
