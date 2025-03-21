% Name: James Weng
% UvAnetID: 15685365
% Study: BSc Informatica
% This file creates a game gen server that maintains a game of dots and boxes.
% The state contains the grid and the players. The player in the head of the
% list is the one that has to make a move.

-module(game_server).

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2, handle_continue/2]).
-export([init/1, move/2]).

% Create a game server.
start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).

% Informs the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    [H|_] = Players,
    H ! {move, self(), Grid},
    {ok, {Grid, Players}}.

% Handle move from player and return the score of the move.
handle_call({move, Wall}, _From, State) ->
    {Grid, [H|T]} = State,
    case grid:has_wall(Wall, Grid) or not grid:is_valid_wall(Wall, Grid)of
        true -> {reply, {ok, 0}, {Grid, T ++ [H]}};
        false ->
            NewGrid = grid:add_wall(Wall, Grid),
            {{X, Y}, {A, B}} = Wall,
            {_, _, Walls} = NewGrid,
            case {grid:get_cell_walls(X, Y) -- Walls, grid:get_cell_walls(A, B) -- Walls} of
                {[], []} -> {reply, {ok, 2}, {NewGrid, [H|T]}, {continue, check_grid}};
                {_, []} -> {reply, {ok, 1}, {NewGrid, [H|T]}, {continue, check_grid}};
                {[], _} -> {reply, {ok, 1}, {NewGrid, [H|T]}, {continue, check_grid}};
                {_, _} -> {reply, {ok, 0}, {NewGrid, T ++ [H]}, {continue, check_grid}}
            end
    end;

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Check if the game is finished, otherwise tells next player to move.
handle_continue(check_grid, State) ->
    {Grid, [H|T]} = State,
    case grid:get_open_spots(Grid) of
        [] ->
            lists:map(fun(X) -> X ! finished end, [H|T]),
            {stop, normal, State};
        _ ->
            H ! {move, self(), Grid},
            {noreply, State}
    end.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
