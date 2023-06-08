-module(prop_uuid).
-include_lib("proper/include/proper.hrl").

prop_binary_codec() ->
    ?FORALL(Val, proper_types:binary(16),
            proper_lib:codec(pg_uuid, #{uuid_format => binary}, Val)).

prop_string_codec() ->
    ?FORALL(Val, proper_types:binary(16),
            proper_lib:codec(
              pg_uuid, #{uuid_format => string},
              lists:flatten(strfmt(Val)),
              fun canonic_str/1)).

prop_bin_string_codec() ->
    ?FORALL(Val, proper_types:binary(16),
            proper_lib:codec(
              pg_uuid, #{uuid_format => string},
              iolist_to_binary(strfmt(Val)),
              fun canonic_str/1)).

prop_integer_codec() ->
    ?FORALL(Val, proper_types:integer(
                   -170141183460469231731687303715884105728,
                   170141183460469231731687303715884105728),
            proper_lib:codec(pg_uuid, #{uuid_format => integer}, Val)).


strfmt(<<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
    io_lib:format(Format, [U0, U1, U2, U3, U4]).

canonic_str(Str) when is_list(Str) -> Str;
canonic_str(Bin) when is_binary(Bin) -> binary_to_list(Bin).
