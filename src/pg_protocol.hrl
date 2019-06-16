-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).
-define(int64, 1/big-signed-unit:64).

-define(JSONB_VERSION_1, 1).

% from pg_type.h
-define(BOOLOID, 16).
-define(BYTEAOID, 17).
-define(CHAROID, 18).
-define(NAMEOID, 19).
-define(INT8OID, 20).
-define(INT2OID, 21).
-define(INT2VECTOROID, 22).
-define(INT4OID, 23).
-define(REGPROCOID, 24).
-define(TEXTOID, 25).
-define(OIDOID, 26).
-define(TIDOID, 27).
-define(XIDOID, 28).
-define(CIDOID, 29).
-define(OIDVECTOROID, 30).
-define(JSONOID, 114).
-define(JSONBOID, 3802).
-define(XMLOID, 142).
-define(PGNODETREEOID, 194).
-define(POINTOID, 600).
-define(LSEGOID, 601).
-define(PATHOID, 602).
-define(BOXOID, 603).
-define(POLYGONOID, 604).
-define(LINEOID, 628).
-define(CIDRARRAYOID, 651).         % not #defined
-define(FLOAT4OID, 700).
-define(FLOAT8OID, 701).
-define(ABSTIMEOID, 702).
-define(RELTIMEOID, 703).
-define(TINTERVALOID, 704).
-define(UNKNOWNOID, 705).
-define(CIRCLEOID, 718).
-define(CASHOID, 790).
-define(MACADDROID, 829).
-define(INETOID, 869).
-define(CIDROID, 650).
-define(BOOLARRAYOID, 1000).        % not #defined
-define(BYTEAARRAYOID, 1001).       % not #defined
-define(CHARARRAYOID, 1002).        % not #defined
-define(NAMEARRAYOID, 1003).        % not #defined
-define(INT2ARRAYOID, 1005).        % not #defined
-define(INT2VECTORARRAYOID, 1006).  % not #defined
-define(INT4ARRAYOID, 1007).
-define(REGPROCARRAYOID, 1008).     % not #defined
-define(TEXTARRAYOID, 1009).
-define(TIDARRAYOID, 1010).         % not #defined
-define(XIDARRAYOID, 1011).         % not #defined
-define(CIDARRAYOID, 1012).         % not #defined
-define(OIDVECTORARRAYOID, 1013).   % not #defined
-define(BPCHARARRAYOID, 1014).      % not #defined
-define(VARCHARARRAYOID, 1015).     % not #defined
-define(INT8ARRAYOID, 1016).        % not #defined
-define(POINTARRAYOID, 1017).       % not #defined
-define(LSEGARRAYOID, 1018).        % not #defined
-define(PATHARRAYOID, 1019).        % not #defined
-define(BOXARRAYOID, 1020).         % not #defined
-define(FLOAT4ARRAYOID, 1021).
-define(FLOAT8ARRAYOID, 1022).      % not #defined
-define(ABSTIMEARRAYOID, 1023).     % not #defined
-define(RELTIMEARRAYOID, 1024).     % not #defined
-define(TINTERVALARRAYOID, 1025).   % not #defined
-define(POLYGONARRAYOID, 1027).     % not #defined
-define(OIDARRAYOID, 1028).         % not #defined
-define(ACLITEMOID, 1033).
-define(ACLITEMARRAYOID, 1034).     % not #defined
-define(MACADDRARRAYOID, 1040).     % not #defined
-define(INETARRAYOID, 1041).        % not #defined
-define(BPCHAROID, 1042).
-define(VARCHAROID, 1043).
-define(DATEOID, 1082).
-define(TIMEOID, 1083).
-define(TIMESTAMPOID, 1114).
-define(TIMESTAMPTZOID, 1184).
-define(INTERVALOID, 1186).
-define(CSTRINGARRAYOID, 1263).
-define(TIMETZOID, 1266).
-define(BITOID, 1560).
-define(VARBITOID, 1562).
-define(NUMERICOID, 1700).
-define(REFCURSOROID, 1790).
-define(REGPROCEDUREOID, 2202).
-define(REGOPEROID, 2203).
-define(REGOPERATOROID, 2204).
-define(REGCLASSOID, 2205).
-define(REGTYPEOID, 2206).
-define(REGTYPEARRAYOID, 2211).
-define(UUIDOID, 2950). % not #defined
-define(UUIDARRAYOID, 2951). % not #defined
-define(TSVECTOROID, 3614).
-define(GTSVECTOROID, 3642).
-define(TSQUERYOID, 3615).
-define(REGCONFIGOID, 3734).
-define(REGDICTIONARYOID, 3769).
-define(INT4RANGEOID, 3904).
-define(INT8RANGEOID, 3926).
-define(NUMRANGEOID, 3906).
-define(TSTZRANGEOID, 3910).
-define(TSRANGEOID, 3908).
-define(DATERANGEOID, 3912).
-define(ANYRANGEOID, 3831).

-define(RECORDOID, 2249).
-define(RECORDARRAYOID, 2287).
-define(CSTRINGOID, 2275).
-define(ANYOID, 2276).
-define(ANYARRAYOID, 2277).
-define(VOIDOID, 2278).
-define(TRIGGEROID, 2279).
-define(LANGUAGE_HANDLEROID, 2280).
-define(INTERNALOID, 2281).
-define(OPAQUEOID, 2282).
-define(ANYELEMENTOID, 2283).
-define(ANYNONARRAYOID, 2776).
-define(ANYENUMOID, 3500).
-define(FDW_HANDLEROID, 3115).


%% Commands defined as per this page:
%% https://www.postgresql.org/docs/current/static/protocol-message-formats.html

%% Commands
-define(BIND, $B).
-define(CLOSE, $C).
-define(DESCRIBE, $D).
-define(EXECUTE, $E).
-define(FLUSH, $H).
-define(PASSWORD, $p).
-define(PARSE, $P).
-define(SIMPLEQUERY, $Q).
-define(AUTHENTICATION_REQUEST, $R).
-define(SYNC, $S).
-define(SASL_ANY_RESPONSE, $p).

%% Parameters

-define(PREPARED_STATEMENT, $S).
-define(PORTAL, $P).

%% Responses

-define(PARSE_COMPLETE, $1).
-define(BIND_COMPLETE, $2).
-define(CLOSE_COMPLETE, $3).
-define(NOTIFICATION, $A).
-define(COMMAND_COMPLETE, $C).
-define(DATA_ROW, $D).
-define(ERROR, $E).
-define(EMPTY_QUERY, $I).
-define(CANCELLATION_KEY, $K).
-define(NO_DATA, $n).
-define(NOTICE, $N).
-define(PORTAL_SUSPENDED, $s).
-define(PARAMETER_STATUS, $S).
-define(PARAMETER_DESCRIPTION, $t).
-define(ROW_DESCRIPTION, $T).
-define(READY_FOR_QUERY, $Z).
-define(COPY_BOTH_RESPONSE, $W).
-define(COPY_DATA, $d).

% CopyData replication messages
-define(X_LOG_DATA, $w).
-define(PRIMARY_KEEPALIVE_MESSAGE, $k).
-define(STANDBY_STATUS_UPDATE, $r).
