% Name: James Weng
% UvAnetID: 15685365
% Study: BSc Informatica
% This file creates a client process used in the dots and boxes game.

-module(client).
-export([move/0, new/0]).

move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsss,{S1, S2, S3}),
    receive
        finished ->
            io:format("~p: I am done~n", [self()]);
        {move, ServerPid, Grid} ->
            case grid:get_completable_walls(Grid) of
                [] -> gen_server:call(ServerPid, {move, grid:choose_random_wall(Grid)});
                [H|_] -> gen_server:call(ServerPid, {move, H})
            end,
            move()
    end.

new() ->
    spawn(client, move, []).
