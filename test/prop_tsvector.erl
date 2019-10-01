-module(prop_tsvector).
-include_lib("proper/include/proper.hrl").


prop_codec() ->
    ?FORALL(Val, tsvector(),
            proper_lib:codec(pg_tsvector, [], Val)).

tsvector() ->
    proper_types:list(lexeme()).

lexeme() ->
    {str(),                                     % word
     proper_types:list(position())}.            % positions

position() ->
    {proper_types:integer(0, 8192),              % position
     proper_types:oneof(['A', 'B', 'C', null])}. % weight

str() ->
    ?LET(
       NoZeroStr,
       ?SUCHTHAT(S, proper_types:list(proper_types:byte()),
                 not lists:member(0, S)),
       list_to_binary(NoZeroStr)).
