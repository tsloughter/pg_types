-define(int8, 1/big-signed-unit:8).
-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).
-define(int64, 1/big-signed-unit:64).
-define(int128, 1/big-signed-unit:128).

-define(float32, 1/big-float-unit:32).
-define(float64, 1/big-float-unit:64).

-define(badarg(Reason), {badarg, {?MODULE, Reason}}).
