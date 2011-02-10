
-module(flashpolicy_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-define(DEFAULT_FILE, "../flashpolicy.xml").
-define(DEFAULT_LOGGING, false).
-define(DEFAULT_ADDRESS, any).
-define(DEFAULT_PORT, 843).

start() ->
  application:load(flashpolicy),
  application:start(flashpolicy).  

start(_StartType, _StartArgs) ->
  PolicyFile     = get_env_with_default(policy_file, ?DEFAULT_FILE),
  ListenAddress  = get_env_with_default(listen_at_interface, ?DEFAULT_ADDRESS),
  Port           = get_env_with_default(port, ?DEFAULT_PORT),
  LoggingEnabled = get_env_with_default(enable_logging, ?DEFAULT_LOGGING),
  
  case flashpolicy_sup:start_link(PolicyFile, ListenAddress, Port, LoggingEnabled) of
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