-module(pg_interval).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"interval_send">>], []}.

encode({interval, {T, D, M}}, _) ->
    <<(pg_timestamp:encode_time(T)):64, D:32, M:32>>;
encode({T, D, M}, _) ->
    <<(pg_timestamp:encode_time(T)):64, D:32, M:32>>.

decode(Time, _) ->
    pg_time:decode_time(Time).

%% encode_parameter({interval, {T, D, M}}, _, _OIDMap, true) ->
%%     <<16:32/integer, (encode_time(T, true)):64, D:32, M:32>>;
%% encode_parameter({T, D, M}, ?INTERVALOID, _OIDMap, true) ->
%%     <<16:32/integer, (encode_time(T, true)):64, D:32, M:32>>;
%% encode_parameter({T, D, M}, ?INTERVALOID, _OIDMap, false) ->
%%     <<16:32/integer, (encode_time(T, false)):1/big-float-unit:64, D:32, M:32>>;
