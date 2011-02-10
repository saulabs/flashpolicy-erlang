{application, flashpolicy, [
  {description, "Erlang Flash Policy File Server"},
  {vsn, "1.0"},
  {modules, [flashpolicy_app, flashpolicy_sup, flashpolicy_srv]},
  {registered, [flashpolicy_app, flashpolicy_srv]},
  {applications, [kernel, stdlib]},
  {mod, {flashpolicy_app,[]}},
  {env, [  
    {policy_file, "./flashpolicy.xml"},      
    {enable_logging, false},                    %% true | false
    {listen_at_interface, any},                 %% any | e.g. {192,168,0,2}
    {port, 843}]}
]}.