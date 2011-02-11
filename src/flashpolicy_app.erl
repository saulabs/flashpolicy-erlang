
-module(flashpolicy_app).

-behaviour(application).

-include("socket_config.hrl").

-export([start/0, start/2, stop/1]).

-define(DEFAULT_FILE, "./flashpolicy.xml").
-define(DEFAULT_LOGGING, false).
-define(DEFAULT_ADDRESS, any).
-define(DEFAULT_PORT, 843).

start() ->
  application:load(flashpolicy),
  application:start(flashpolicy).  

start(_StartType, _StartArgs) ->

  ServerConfig = #socket_config {
    bind_address    = get_env_with_default(listen_at_interface, ?DEFAULT_ADDRESS),
    bind_port       = get_env_with_default(port, ?DEFAULT_PORT),
    policy_file     = get_env_with_default(policy_file, ?DEFAULT_FILE),
    logging_enabled = get_env_with_default(enable_logging, ?DEFAULT_LOGGING)
  },

  ConfigForAdditionalServers = [
    #socket_config {
      bind_address    = Interface,
      bind_port       = Port,
      policy_file     = PolicyFile,
      logging_enabled = get_env_with_default(enable_logging, ?DEFAULT_LOGGING)
    } || {Interface, Port, PolicyFile} <- get_env_with_default(bind_also_at, [])],
  
  case flashpolicy_sup:start_link([ServerConfig | ConfigForAdditionalServers]) of
    Success = {ok, _Pid}       -> Success;
    Error   = {error, _Reason} -> Error;
    Unknown                    -> {error, Unknown}
  end.


get_env_with_default(EnvKey, DefaultValue) ->
  case application:get_env(flashpolicy, EnvKey) of
    {ok, Value} -> Value;
    undefined   -> DefaultValue
  end.


stop(_State) ->
  ok.