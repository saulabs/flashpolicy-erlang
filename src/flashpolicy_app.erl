
-module(flashpolicy_app).

-behaviour(application).

-include("socket_config.hrl").

% application callbacks
-export([start/2, stop/1]).
% remote commands
-export([status/1, shutdown/1, execute/1]).
% controlling the server
-export([start/0, enable_logging/1, reload_policy_files/0]).


-define(DEFAULT_POLICY_FILE, "./flashpolicy.xml").
-define(DEFAULT_CERTIFICATE_FILE, "./ssl/host.cert").
-define(DEFAULT_KEY_FILE, "./ssl/host.key").
-define(DEFAULT_LOG_PATH, "./log/").
-define(DEFAULT_LOGGING, false).
-define(DEFAULT_ADDRESS, any).
-define(DEFAULT_PORT, 843).

%------------------------------------------------------------------------------
% API / EXPORTS
%------------------------------------------------------------------------------

start() ->
  application:load(flashpolicy),
  application:start(flashpolicy).

%% @doc enables or disables logging to file
-spec enable_logging(Enable::boolean()) -> ok | alread_enabled | already_disabled | {error, Reason::term()}.
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

reload_policy_files() ->
  [gen_server:cast(ChildPid, reload_policy_file) || {_Id, ChildPid, _Type, _Modules} <- supervisor:which_children(flashpolicy_sup)].

%------------------------------------------------------------------------------
% APPLICATION CALLBACKS
%------------------------------------------------------------------------------

start(_StartType, _StartArgs) ->

  ServerConfig = #socket_config {
    bind_address    = get_env_with_default(listen_at_interface, ?DEFAULT_ADDRESS),
    bind_port       = get_env_with_default(port, ?DEFAULT_PORT),
    policy_file     = get_env_with_default(policy_file, ?DEFAULT_POLICY_FILE),
    cert_file       = CertFile = get_env_with_default(cert_file, ?DEFAULT_CERTIFICATE_FILE),
    key_file        = KeyFile  = get_env_with_default(key_file, ?DEFAULT_KEY_FILE),
    key_pwd         = KeyPwd   = get_env_with_default(key_pwd, none),
    intermediate_cert_file = IntermediateCertFile = get_env_with_default(intermediate_cert_file, none)    
  },

  ConfigForAdditionalServers = [
    #socket_config {
      bind_address    = Interface,
      bind_port       = Port,
      policy_file     = PolicyFile,
      cert_file       = CertFile,
      key_file        = KeyFile,
      key_pwd         = KeyPwd,
      intermediate_cert_file = IntermediateCertFile            
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


%------------------------------------------------------------------------------
% REMOTE COMMANDS - EXECUTED ON REMOTE ERLANG NODE HOSTING THE SERVER
%------------------------------------------------------------------------------

%% @doc connects to the given node running the flash policy server and prints status
-spec status(NodeName::string()) -> ok | error.
status([NodeName]) when is_list(NodeName) ->
  Node = list_to_atom(NodeName),
  io:format("connecting to ~p ...", [Node]),
  case net_adm:ping(Node) of
    pong ->
      io:format(" OK~n"),
      case catch rpc:call(Node, supervisor, which_children, [flashpolicy_sup]) of
        Children = [_|_] ->
          io:format("found ~b policy server processes.~n", [length(Children)]),
          [case catch gen_server:call(ChildPid, status) of
              {SocketConfig = #socket_config{}, AcceptedClients} when is_list(AcceptedClients) ->
                SocketConfig = #socket_config{bind_address = BindAddress, bind_port = Port, policy_file = PolicyFile} = SocketConfig,
                Address = case BindAddress of
                  any -> "any interface at port ";
                  {A,B,C,D} when is_integer(A), is_integer(B), is_integer(C), is_integer(D) -> integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D) ++ ":";
                  _ -> "unknown interface at port"
                end,
                io:format("serving file ~s at ~s~b. ~b open connections.~n", [PolicyFile, Address, Port, length(AcceptedClients)]);
              _Error ->
                io:format("No info available for ~p~n", [ChildPid]), error
            end || {_Id, ChildPid, _Type, _Modules} <- Children], ok;
          _Error -> io:format("No server processes found!~n"), halt(1)
      end;
    pang ->
      io:format(" FAILED~n"), halt(1)
  end.

%% @doc connects to the given node running the flash policy server and prints status
-spec execute(NodeName::string()) -> ok | error.
execute([NodeName, Command]) when is_list(NodeName), is_list(Command) ->
  Node = list_to_atom(NodeName),
  case net_adm:ping(Node) of
    pong ->
      case Command of
        "reload" ->
          case catch rpc:call(Node, ?MODULE, reload_policy_files, []) of
            [_|_] -> io:format("reloaded policy files.~n"), ok;
              _Error -> io:format("reload failed!~n"), halt(1)
          end;
        "enable-logging" ->
          case catch rpc:call(Node, ?MODULE, enable_logging, [true]) of
            ok -> io:format("logging enabled.~n"), ok;
            alread_enabled -> io:format("logging (already) enabled.~n"), ok;
              _Error -> io:format("enable logging failed!~n"), halt(1)
          end;
        "disable-logging" ->
          case catch rpc:call(Node, ?MODULE, enable_logging, [false]) of
            ok -> io:format("logging disabled.~n"), ok;
            already_disabled ->  io:format("logging (already) disabled.~n"), ok;
              _Error -> io:format("disable logging failed!~n"), halt(1)
          end;
        _ -> io:format("unknown command~n"), halt(1)
      end;
    pang ->
      io:format("server is not running~n"), halt(1)
  end.

%% @doc shuts down the node that is running the flash policy server
-spec shutdown(NodeName::string()) -> ok | error.
shutdown([NodeName]) when is_list(NodeName) ->
  Node = list_to_atom(NodeName),
  case net_adm:ping(Node) of
    pong ->
      io:format("shutting down ~p ... ", [Node]),
      monitor_node(Node, true),
      case catch rpc:call(Node, init, stop, []) of
        ok ->
          receive
            {nodedown, Node} -> io:format("OK~n"), ok
            after 10000 -> io:format("timed out~n"), halt(1)
          end;
        _Error -> io:format("failed~n"), halt(1)
      end;
    pang ->
      io:format("node ~p is already down.", [Node])
  end.

