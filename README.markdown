
Erlang Flash Policy File Server
==============================

###Erlang Server serving flash crossdomain policies on adobe standard port 843, intended for use in production environments.

This policy server accepts **Policy File Requests** from Flash Movies that use the **[flash.net.XMLSocket](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/XMLSocket.html) Class**. These requests are sent by the Flash Player automatically, if the Flash Movie tries to connect to a different host as the origin of the Flash Movie.

**If you use the [flash.net.SecureSocket](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/SecureSocket.html) Class** to establish a **SSL encrypted connection to your Server**, the Flash Player **requests the master policy** at Port 843 **also SSL encrypted**, which is not very well documented by Adobe. **In that case** you should **use the [ssl branch](https://github.com/saulabs/flashpolicy-erlang/tree/ssl)** of this project, otherwise the policy file requests will fail.


* ####Building Server:

        ./build.sh
        
* ####Configure Policy File:

    Edit _flashpolicy.xml_ and replace `<allow-access-from domain="*" to-ports="123" />` __port__ 123 with a comma separated list of ports your flash client should be allowed to connect at, and maybe replace the wildcard __domain__ with the domain that serves your flash file.

* Starting __Production__ Server:

        sudo ./policyserver start
        
      Notice: You must be __root__ to start the server because it binds by default to the __privileged port__ 843.

* ####Testing Server:


        perl -e 'printf "<policy-file-request/>%c",0' | nc 127.0.0.1 843
        
        # or
        
        ./policyserver test

* ####Reload Policy File:
   
    If you __modified the _flashpolicy.xml_ file__, you can __reload__ it during runtime using `./policyserver reload`.  
        
* ####Extended Server Configuration:

    The server can be configured in the _env_ section of either the _src/flashpolicy.app.src_ file or the _ebin/flashpolicy.app_ file. If you edit the _src/flashpolicy.app.src_ file you must run `./build.sh` again, that will copy it to the _ebin_ directory and __overwrite__ the _ebin/flashpolicy.app_ file. __Changing__ the server __configuration__ requires a __server restart__: `./policyserver stop && sudo ./policyserver start`.
   
          {env, [
            {policy_file, "./flashpolicy.xml"},  %% string(): policyfile to serve
            {enable_logging, true},              %% boolean(): enable or disable logging
            {logfile_path, "./log/"},            %% string(): path to logfiles. must end with path separator '/'
            {listen_at_interface, any},          %% any | e.g. {192,168,0,2}: the ip address as tuple to bind at, or 'any' to listen at all interfaces
            {port, 843},                         %% integer(): the port to listen at
        
            {bind_also_at, []}                   %% [{interface(), port(), policy_file()}]: additional interfaces and ports to listen at, e.g [{any, 8080, "./otherPolicy.xml"}]
          ]}

  
    _bind_also_at_ can be used to serve different policy files at different ports or interfaces.
  
* ####Logging:

    Logging can be enabled or disabled during runtime using `./policyserver enable-logging` or `./policyserver disable-logging`. The default logging directory is _./log_.

