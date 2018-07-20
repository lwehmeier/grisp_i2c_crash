% @doc helloworld public API.
% @end
-module(helloworld).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).


-define(PYNODE, 'py@rpi3').
-define(PYPROCESS, pyBridge).
%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) ->
    {ok, Supervisor} = helloworld_sup:start_link(),
    application:start(grisp),
    {ok, Supervisor}.
stop(_State) -> ok.
