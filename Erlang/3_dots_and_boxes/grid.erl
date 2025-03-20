-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2,
    get_cell_walls/2, get_all_walls/2, get_open_spots/1, choose_random_wall/1]).

new(Width, Height) ->
    {Width, Height, []}.

get_wall(X, Y, Dir) -> case Dir of
    north -> {{X, Y-1}, {X, Y}};
    east -> {{X, Y}, {X+1, Y}};
    south -> {{X, Y}, {X, Y+1}};
    west -> {{X-1, Y}, {X, Y}}
end.

has_wall(Wall, Grid) ->
    {_, _, Walls} = Grid,
    lists:member(Wall, Walls).

add_wall(Wall, Grid) -> case has_wall(Wall, Grid) of
    true -> Grid;
    false ->
        {Width, Height, Walls} = Grid,
        {Width, Height, [Wall|Walls]}
end.

show_hlines(Row, Grid) ->
    {Width, _, _} = Grid,
    lists:flatmap(fun(X) ->
        case has_wall(get_wall(X, Row, north), Grid) of
            true -> "+--";
            false -> "+  "
        end
    end, lists:seq(0, Width-1)) ++ "+~n".

show_vlines(Row, Grid) ->
    {Width, _, _} = Grid,
    lists:flatten(lists:join("  ", lists:map(fun(X) ->
        case has_wall(get_wall(X, Row, west), Grid) of
            true -> "|";
            false -> " "
        end
    end, lists:seq(0, Width))), "~n").

% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
    {_, H, _} = Grid,
    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),

        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),
    io:fwrite("~n"),
    ok.

get_cell_walls(X, Y) ->
    [get_wall(X, Y, Dir) || Dir <- [north, east, south, west]].

get_all_walls(W, H) ->
    [get_wall(X, Y, north) || X <- lists:seq(0, W-1), Y <- lists:seq(0, H)]
    ++ [get_wall(X, Y, west) || X <- lists:seq(0, W), Y <- lists:seq(0, H-1)].

get_open_spots(Grid) ->
    {W, H, Walls} = Grid,
    lists:subtract(get_all_walls(W, H), Walls).

choose_random_wall(Grid) -> case get_open_spots(Grid) of
    [] -> [];
    L -> lists:nth(rand:uniform(length(L)), L)
end.
