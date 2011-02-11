
-module(flashpolicy_sup).

-behaviour(supervisor).

-include("socket_config.hrl").

-export([start_link/1]).
-export([init/1]).


start_link(ServerConfigs = [#socket_config{} | _]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ServerConfigs).

init(ServerConfigs = [#socket_config{} | _]) ->
  process_flag(trap_exit, true),
  ChildSpec = [
    {flashpolicy_srv:registered_server_name(ListenAddress, Port), 
    {flashpolicy_srv, start_link, [Configuration]}, 
    transient, 5000, worker, [flashpolicy_srv]} || 
    Configuration = #socket_config{ 
      bind_address    = ListenAddress,
      bind_port       = Port
    } <- ServerConfigs],
  {ok, {{one_for_one, 10, 1}, ChildSpec}}.

