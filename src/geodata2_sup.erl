-module(geodata2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% supervisor behavior
-export([init/1]).

-include("geodata2.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildName = child_proc,
    ChildSpec = {ChildName,
                 {geodata2, start_link, [ChildName]},
                 permanent,
                 5000,
                 worker,
                 [geodata2]},

    {ok, {SupFlags, [ChildSpec]}}.
