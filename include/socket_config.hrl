
-record(socket_config, {
  bind_address,     %% any | {A,B,C,D}: address to bind at. either atom any to listen at all interfaces or ip address as tuple
  bind_port,        %% integer(): the port to listen at
  policy_file       %% list(): the name of the policy file to serve
}).