% @doc helloworld top level supervisor.
% @end
-module(helloworld_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
  Children = [
    ],
  supervisor:start_link({local, ?MODULE}, ?MODULE, Children).

%--- Callbacks -----------------------------------------------------------------

init([]) -> {ok, { {one_for_all, 0, 1}, []} }.
