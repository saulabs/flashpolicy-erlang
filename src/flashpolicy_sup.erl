
-module(flashpolicy_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).


start_link(PolicyFile = [_|_], ListenAddress, Port, LoggingEnabled) when is_integer(Port), is_boolean(LoggingEnabled) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [PolicyFile, ListenAddress, Port, LoggingEnabled]).

init([PolicyFile = [_|_], ListenAddress, Port, LoggingEnabled]) when is_integer(Port), is_boolean(LoggingEnabled) ->
    process_flag(trap_exit, true),
    ChildSpec = [{flashpolicy_srv, {flashpolicy_srv, start_link, [PolicyFile, ListenAddress, Port, LoggingEnabled]}, transient, 5000, worker, [flashpolicy_srv]}],
    {ok, {{one_for_one, 10, 1}, ChildSpec}}.
