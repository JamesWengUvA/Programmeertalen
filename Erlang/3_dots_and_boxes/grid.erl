% Name: James Weng
% UvAnetID: 15685365
% Study: BSc Informatica
% This file contains functions for creating and changing a grid for the
% implementation of the game dots and boxes.

-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2,
    add_wall/2, get_cell_walls/2, get_all_walls/2, get_open_spots/1,
    choose_random_wall/1]).

% Return a new grid.
new(Width, Height) ->
    {Width, Height, []}.

% Return a wall using the coordinates of one of the
% neighbouring cells and a cardinal direction.
get_wall(X, Y, Dir) -> case Dir of
    north -> {{X, Y-1}, {X, Y}};
    east -> {{X, Y}, {X+1, Y}};
    south -> {{X, Y}, {X, Y+1}};
    west -> {{X-1, Y}, {X, Y}}
end.

% Check if the wall is placed in the grid.
has_wall(Wall, Grid) ->
    {_, _, Walls} = Grid,
    lists:member(Wall, Walls).

% Add a wall in the grid.
add_wall(Wall, Grid) -> case has_wall(Wall, Grid) of
    true -> Grid;
    false ->
        {Width, Height, Walls} = Grid,
        {Width, Height, [Wall|Walls]}
end.

% Helper function to print the horizontal walls.
show_hlines(Row, Grid) ->
    {Width, _, _} = Grid,
    lists:flatmap(fun(X) ->
        case has_wall(get_wall(X, Row, north), Grid) of
            true -> "+--";
            false -> "+  "
        end
    end, lists:seq(0, Width-1)) ++ "+~n".

% Helper function to prind the vertical walls.
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

% Return a list of the possible walls neighbouring a cell.
get_cell_walls(X, Y) ->
    [get_wall(X, Y, Dir) || Dir <- [north, east, south, west]].

% Return a list of all possible walls of a grid.
get_all_walls(W, H) ->
    [get_wall(X, Y, north) || X <- lists:seq(0, W-1), Y <- lists:seq(0, H)]
    ++ [get_wall(X, Y, west) || X <- lists:seq(0, W), Y <- lists:seq(0, H-1)].

% Return a list of all empty walls in a grid.
get_open_spots(Grid) ->
    {W, H, Walls} = Grid,
    lists:subtract(get_all_walls(W, H), Walls).

% Choose a random empty wall in the grid.
choose_random_wall(Grid) -> case get_open_spots(Grid) of
    [] -> [];
    L -> lists:nth(rand:uniform(length(L)), L)
end.
