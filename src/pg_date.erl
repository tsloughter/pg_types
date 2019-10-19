-module(pg_date).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-define(POSTGRESQL_GD_EPOCH, 730485). % ?_value(calendar:date_to_gregorian_days({2000,1,1}))).

init(_Opts) ->
    {[<<"date_send">>], []}.

encode(Date, _TypeInfo) ->
    <<4:?int32, (encode_date(Date)):?int32>>.

decode(<<Date:?int32>>, _TypeInfo) ->
    calendar:gregorian_days_to_date(Date + ?POSTGRESQL_GD_EPOCH).

encode_date(Date) ->
    calendar:date_to_gregorian_days(Date) - ?POSTGRESQL_GD_EPOCH.
