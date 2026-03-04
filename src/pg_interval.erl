-module(pg_interval).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2,
         type_spec/0]).

-include("pg_types.hrl").
-include("pg_protocol.hrl").

init(#{interval_config := raw}) ->
    {[<<"interval_send">>], raw};
init(_Opts) ->
    Config = application:get_env(pg_types, interval_config, []),
    {[<<"interval_send">>], Config}.

encode({interval, Microseconds, Days, Months}, _) when is_integer(Microseconds),
                                                        is_integer(Days),
                                                        is_integer(Months) ->
    <<16:?int32, Microseconds:?int64, Days:?int32, Months:?int32>>;
encode({interval, {T, D, M}}, TypeInfo) ->
    encode({T, D, M}, TypeInfo);
encode({T, D, M}, _) ->
    <<16:?int32, (pg_time:encode_time(T)):?int64, D:?int32, M:?int32>>.

decode(<<Microseconds:?int64, Days:?int32, Months:?int32>>, #type_info{config=raw}) ->
    {interval, Microseconds, Days, Months};
decode(<<T:?int64, D:?int32, M:?int32>>, _) ->
    {interval, {pg_time:decode_time(<<T:?int64>>), D, M}}.

%% encode_parameter({interval, {T, D, M}}, _, _OIDMap, true) ->
%%     <<16:32/integer, (encode_time(T, true)):64, D:32, M:32>>;
%% encode_parameter({T, D, M}, ?INTERVALOID, _OIDMap, true) ->
%%     <<16:32/integer, (encode_time(T, true)):64, D:32, M:32>>;
%% encode_parameter({T, D, M}, ?INTERVALOID, _OIDMap, false) ->
%%     <<16:32/integer, (encode_time(T, false)):1/big-float-unit:64, D:32, M:32>>;

type_spec() ->
    "{interval, Microseconds::integer(), Days::integer(), Months::integer()} | "
    "{interval, {{Hours::integer(), Minutes::integer(), "
    "Seconds::integer()}, Days::integer(), Months::integer()}}".
