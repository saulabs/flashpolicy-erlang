
-module(flashpolicy_app).

-behaviour(application).

-include("socket_config.hrl").

-export([start/2, stop/1]).
-export([start/0, enable_logging/1]).

-define(DEFAULT_FILE, "./flashpolicy.xml").
-define(DEFAULT_LOG_PATH, "./log/").
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
    policy_file     = get_env_with_default(policy_file, ?DEFAULT_FILE)
  },

  ConfigForAdditionalServers = [
    #socket_config {
      bind_address    = Interface,
      bind_port       = Port,
      policy_file     = PolicyFile
    } || {Interface, Port, PolicyFile} <- get_env_with_default(bind_also_at, [])],
    
  enable_logging(get_env_with_default(enable_logging, ?DEFAULT_LOGGING)),
  
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

enable_logging(_Enable = false) ->
  case error_logger:logfile(filename) of
    {error, no_log_file} -> already_disabled;
    Error = {error, _Reason} -> Error;
    _FileName -> error_logger:logfile(close)
  end;  
enable_logging(_Enable = true) ->
  case error_logger:logfile(filename) of
    {error, _} ->
      LogFilePath = get_env_with_default(logfile_path, ?DEFAULT_LOG_PATH),
      LogFileName = lists:flatten(LogFilePath ++ io_lib:format("~4..0B_~02..0B_~2..0B_~2..0B_~2..0B_~2..0B.log",tuple_to_list(erlang:date()) ++ tuple_to_list(erlang:time()))),
      error_logger:logfile({open, LogFileName });
    _FileName -> alread_enabled
  end.
