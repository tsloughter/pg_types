%% https://www.postgresql.org/docs/current/datatype-geometric.html
-module(prop_geometry).
-include_lib("proper/include/proper.hrl").

prop_point_codec() ->
    ?FORALL(Val, point(),
            proper_lib:codec(pg_point, [], Val)).


prop_line_codec() ->
    ?FORALL(Val, line(),
            proper_lib:codec(pg_line, [], Val)).


prop_line_segment_codec() ->
    ?FORALL({P1, P2}, {point(), point()},
            proper_lib:codec(pg_line_segment, [],
                             #{point1 => P1, point2 => P2})).

%% prop_box_codec() ->
%%     not_implemented.

prop_path_codec() ->
    ?FORALL({Open, Points},
            {proper_types:boolean(),
             proper_types:list(point())},
            proper_lib:codec(pg_path, [],
                             #{open => Open, points => Points})).


prop_polygon_codec() ->
    ?FORALL(Vertices,
            proper_types:list(point()),
            proper_lib:codec(pg_polygon, [],
                             #{vertices => Vertices})).

prop_circle_codec() ->
    ?FORALL({Center, Radius},
            {point(), coord()},
            proper_lib:codec(pg_circle, [],
                             #{center => Center, radius => Radius})).
%%
%% Types
%%

point() ->
    ?LET({X, Y}, {coord(), coord()},
         #{x => X, y => Y}).

line() ->
    ?LET({A, B, C},
         {coord(),
          coord(),
          coord()},
         #{a => A, b => B, c => C}).

coord() ->
    proper_types:float().
