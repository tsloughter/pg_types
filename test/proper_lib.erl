-module(proper_lib).

-export([codec/3, codec/4, encode_decode/3]).

-export([int16/0, int32/0, int64/0,
         date_gen/0, int_time_gen/0, utc_offset_gen/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("pg_types.hrl").

codec(Mod, Opts, Data) ->
    codec(Mod, Opts, Data, fun(V) -> V end).

% Encode and then decode the Input
encode_decode(Mod, Opts, Input) ->
    {_, Config} = Mod:init(Opts),
    TypeInfo = #type_info{config = Config},
    <<Size:32, Encoded:Size/binary>> = iolist_to_binary(Mod:encode(Input, TypeInfo)),
    Mod:decode(Encoded, TypeInfo).

% Passing a function will apply that function to the input Data and
% encoded/decoded Data before comparison.
codec(Mod, Opts, Data, Canonical) when is_function(Canonical) ->
    Decoded = encode_decode(Mod, Opts, Data),
    Canonical(Data) =:= Canonical(Decoded);
% Passing anything else will simply compare the Output with the encoded/decoded
% Input
codec(Mod, Opts, Data, Output) ->
    Decoded = encode_decode(Mod, Opts, Data),
    Output =:= Decoded.

%%
%% Generators
%%

int16() ->
    proper_types:integer(-32768, 32256).

int32() ->
    proper_types:integer(-2147483648, 2130706432).

int64() ->
    proper_types:integer(-9223372036854775808, 9151314442816847872).

date_gen() ->
    ?SUCHTHAT(
       Date,
       {proper_types:integer(-9999, 9999),
        proper_types:integer(1, 12),
        proper_types:integer(1, 31)},
       calendar:valid_date(Date)).

utc_offset_gen() ->
    {proper_types:integer(-15, 15),
     proper_types:integer(0, 59)}.

int_time_gen() ->
    {proper_types:integer(0, 23),
     proper_types:integer(0, 59),
     proper_types:integer(0, 59)}.
