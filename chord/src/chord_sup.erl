-module(chord_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

-define(SERVER, ?MODULE).


%% TODO: use a name for the chord server. that way chord can be used
%% for multiple things on the same node.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).



init([]) ->
    Server = {chord, {chord_server, start_link, []}, % {ID, {module, function, args}, restart, shutdown, type, modules}
              permanent, 2000, worker, [chord_server, chord]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
