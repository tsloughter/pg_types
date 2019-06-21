-module(pg_point).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

init(_Opts) ->
    {[<<"point_send">>], []}.

encode(#{x := X, y := Y}, _) ->
    <<X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
encode(#{long := X, lat := Y}, _) ->
    <<X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
encode({point, {X, Y}}, _) ->
    <<X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
encode({X, Y}, _) ->
    <<X:1/big-float-unit:64, Y:1/big-float-unit:64>>.

decode(<<X:64/float, Y:64/float>>, _) ->
    %% TODO: make configurable between this and returning #{x => X, y => Y}
    #{long => X, lat => Y}.


%% encode_parameter(#{x := X, y := Y}, ?POINTOID, _OIDMap, _IntegerDateTimes) ->
%%     <<16:32/integer, X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
%% encode_parameter(#{long := X, lat := Y}, ?POINTOID, _OIDMap, _IntegerDateTimes) ->
%%     <<16:32/integer, X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
%% encode_parameter({point, {X, Y}}, ?POINTOID, _OIDMap, _IntegerDateTimes) ->
%%     <<16:32/integer, X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
%% encode_parameter({X, Y}, ?POINTOID, _OIDMap, _IntegerDateTimes) ->
%%     <<16:32/integer, X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
