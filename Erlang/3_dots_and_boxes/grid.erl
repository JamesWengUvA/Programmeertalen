-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2]).

% TODO: The other functions.
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

add_wall(Wall, Grid) ->
    {Width, Height, Walls} = Grid,
    {Width, Height, [Wall|Walls]}.

% TODO
show_hlines(Row, Grid) -> "".
% TODO
show_vlines(Row, Grid) -> "".


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
