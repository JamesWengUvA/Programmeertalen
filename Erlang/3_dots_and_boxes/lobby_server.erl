-module(lobby_server).

-behaviour(gen_server).

-export([start/0, handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, new_game/3, games/0]).

% Create a new game.
new_game(W, H, Players) ->
    gen_server:call(lobby_server, {new_game, W, H, Players}).

% Return the list of games.
games() ->
    gen_server:call(lobby_server, games).

% Start the lobby server.
start() ->
    gen_server:start({local, lobby_server}, lobby_server, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, []}.

% Handle calls by either returning the list of games or adding a new game to the list.
handle_call(games, _From, Games) ->
    {reply, Games, Games};
handle_call({new_game, Width, Height, Players}, _From, Games) ->
    {ok, ServerPid} = game_server:start_link({Width, Height, Players}),
    {reply, {ok, ServerPid}, [ServerPid|Games]}.

% Catch when a game finishes or crashes and remove from the list.
handle_info({'EXIT', ServerPid, _Reason}, Games) ->
    NewGames = lists:delete(ServerPid, Games),
    {noreply, NewGames}.

% Required for gen_server behaviour.
% Normally you would implement this to,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
