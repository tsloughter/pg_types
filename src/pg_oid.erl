-module(pg_oid).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-define(OID_MAX, 4294967295).

init(_Opts) ->
    {[<<"idsend">>, <<"regprocsend">>, <<"regproceduresend">>,
      <<"regopersend">>, <<"regoperatorsend">>, <<"regclasssend">>,
      <<"regtypesend">>, <<"xidsend">>, <<"cidsend">>], []}.

encode(Oid, _) when Oid >= 0 andalso Oid =< ?OID_MAX ->
    <<4:?int32, Oid:?uint32>>;
encode(_Oid, _) ->
    erlang:error({badarg, invalid_oid}).

decode(Name, _) ->
    Name.
