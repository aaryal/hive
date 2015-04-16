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
    Storage = {storage_ets, {storage_ets, start_link, []},
               permanent, 2000, worker, [storage_ets]},
    Server = {chord, {chord, start_link, []},
              permanent, 2000, worker, [chord]},
    Children = [Storage, Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
