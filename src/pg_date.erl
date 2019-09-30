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
    [<<4:?int32>>, <<(encode_date(Date)  - ?POSTGRESQL_GD_EPOCH):?int32>>].

decode(<<Date:?int32>>, _TypeInfo) ->
    decode_date(Date + ?POSTGRESQL_GD_EPOCH).

%% Julian <-> Gregorian
decode_date(N) ->
    J = N + 32044,
    Q1 = J div 146097,
    Extra = (J - Q1 * 146097) * 4 + 3,
    J2 = J + 60 + Q1 * 3 + Extra div 146097,
    Q2 = J2 div 1461,
    J3 = J2 - Q2 * 1461,
    Y = J3 * 4 div 1461,
    J4 = case Y of
        0 -> ((J3 + 306) rem 366) + 123;
        _ -> ((J3 + 305) rem 365) + 123
    end,
    Year = (Y + Q2 * 4) - 4800,
    Q3 = J4 * 2141 div 65536,
    Day = J4 - 7834 * Q3 div 256,
    Month = (Q3 + 10) rem 12 + 1,
    {Year, Month, Day}.

encode_date({Y, M, D}) ->
    M2 = case M > 2 of
        true ->
            M + 1;
        false ->
            M + 13
    end,
    Y2 = case M > 2 of
        true ->
            Y + 4800;
        false ->
            Y + 4799
    end,
    C = Y2 div 100,
    J1 = Y2 * 365 - 32167,
    J2 = J1 + (Y2 div 4 - C + C div 4),
    J2 + 7834 * M2 div 256 + D.
