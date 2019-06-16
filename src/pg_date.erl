-module(pg_date).

-export([typsend/0,
         encode/2,
         decode/2]).

-define(POSTGRESQL_GD_EPOCH, 730485). % ?_value(calendar:date_to_gregorian_days({2000,1,1}))).

typsend() ->
    <<"date_send">>.

encode(Date, _TypeInfo) ->
    encode_date(Date).

decode(<<Date:32/signed-integer>>, _TypeInfo) ->
    calendar:gregorian_days_to_date(Date + ?POSTGRESQL_GD_EPOCH).

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
