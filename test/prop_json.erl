-module(prop_json).
-include_lib("proper/include/proper.hrl").

%% Fake JSON encoder
-export([decode/2, encode/2]).

prop_json_codec() ->
    ?FORALL(Val, json_str(),
            proper_lib:codec(pg_json, [], Val)).

prop_jsonb_codec() ->
    ?FORALL(Val, json_str(),
            proper_lib:codec(pg_jsonb, [], Val)).

prop_custom_json_codec() ->
    Config = #{json_config => {?MODULE, [compressed], []}},
    ?FORALL(Val, json_struct(),
            proper_lib:codec(pg_json, Config, Val)).

prop_custom_jsonb_codec() ->
    Config = #{json_config => {?MODULE, [compressed], []}},
    ?FORALL(Val, json_struct(),
            proper_lib:codec(pg_jsonb, Config, Val)).


%% Fake JSON codec
decode(Bin, _) ->
    binary_to_term(Bin).

encode(Term, Opts) ->
    term_to_binary(Term, Opts).

%% Generators

json_str() ->
    proper_types:binary().


json_struct() ->
    ?LET(KV,
         proper_types:list({j_string(), j_literal()}),
         maps:from_list(KV)).

j_string() ->
    proper_types:string().

j_literal() ->
    proper_types:oneof(
     [j_string(),
      proper_types:integer(1, 10),
      proper_types:float(),
      proper_types:boolean(),
      null]).
