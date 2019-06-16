-record(type_info, {oid :: integer(),
                    pool :: atom(),
                    name :: binary(),
                    typsend :: binary(),
                    typreceive :: binary(),
                    typlen :: integer(),
                    output :: binary(),
                    input :: binary(),
                    array_elem :: integer(),
                    base_oid :: integer(),
                    comp_oids :: integer()}).

-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).
-define(int64, 1/big-signed-unit:64).
