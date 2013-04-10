
%% config for a listening socket that accepts connections
%% from a flash movie that sends policy requests
-record(socket_config, {
  bind_address :: any | {integer(),integer(),integer(),integer()}, %% address to bind at. either atom any to listen at all interfaces or ip address as tuple
  bind_port :: integer(), %% the port to listen at
  policy_file :: string(), %% the name of the policy file to serve
  cert_file :: string(), %% the name of the certificate file
  key_file :: string(), %% the name of the key file
  key_pwd :: string() | none, %% the password for the key file, if it is password protected
  intermediate_cert_file :: string() | none %% the name of the intermediate certificate file
  
}).