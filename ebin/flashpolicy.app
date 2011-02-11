{application, flashpolicy, [
  {description, "Erlang Flash Policy File Server"},
  {vsn, "1.0"},
  {modules, [flashpolicy_app, flashpolicy_sup, flashpolicy_srv]},
  {registered, [flashpolicy_app, flashpolicy_sup]},
  {applications, [kernel, stdlib]},
  {mod, {flashpolicy_app,[]}},
  {env, [  
    {policy_file, "./flashpolicy.xml"},         %% string(): policifile to serve
    {enable_logging, false},                    %% boolean(): enable or disable logging
    {listen_at_interface, any},                 %% any | e.g. {192,168,0,2}: the ip address as tuple to bind at, or 'any' to listen at all interfaces
    {port, 843},                                %% integer(): the port to listen at

    {bind_also_at, []}                          %% [{interface(), port(), policy_file()}]: additional interfaces and ports to listen at, e.g [{any, 8080, "./otherPolicy.xml"}]
  ]}
]}.