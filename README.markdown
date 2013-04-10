
Erlang Flash Policy File Server (SSL)
==============================

###Erlang Server serving flash crossdomain policies on adobe standard port 843 via SSL, intended for use in production environments.

This **branch** accepts **SSL connections from Flash Clients** that use the **[flash.net.SecureSocket](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/SecureSocket.html)**. It is not very well documented by Adobe, but **if you use the [SecureSocket](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/SecureSocket.html) Class** to establish a **SSL encrypted connection to your Server**, the Flash Player **requests the master policy** at Port 843 **also SSL encrypted**. If you use the [flash.net.XMLSocket](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/XMLSocket.html) Class, use the [master branch](https://github.com/saulabs/flashpolicy-erlang) to accept policy requests that are not SSL encrypted.

* ####Building Server:

        ./build.sh
        
* ####Configure Policy File:

    Edit _flashpolicy.xml_ and replace `<allow-access-from domain="*" to-ports="123" />` __port__ 123 with a comma separated list of ports your flash client should be allowed to connect at, and maybe replace the wildcard __domain__ with the domain that serves your flash file.
    
* #### Add you valid Certificate:

    To be able to accept policy requests from Flash Movies via SSL, you must provide a **valid** SSL **Certificate** that must be issued to the domain, the Flash movie wants to connect to. This Certificate and the corresponding (secret) key must be copied to the `ssl` subirectory and named `host.cert` and `host.key`.
    
        cp mydomain.cert flashpolicy-erlang/ssl/host.cert
        cp mydomain-secret.key flashpolicy-erlang/ssl/host.key

    Optionally you can change the name of these files in the `flashpolicy.app` configuration and add an intermediate certificate or an key passphrase. See _Extended Server Configuration_ section for details.  

* Starting __Production__ Server:

        sudo ./policyserver start
        
      Notice: You must be __root__ to start the server because it binds by default to the __privileged port__ 843.

* ####Testing Server:


        perl -e 'printf "<policy-file-request/>%c",0' | openssl s_client -quiet -verify 3 -connect 127.0.0.1:843
        
        # or
        
        ./policyserver test-ssl

* ####Reload Policy File:
   
    If you __modified the _flashpolicy.xml_ file__, you can __reload__ it during runtime using `./policyserver reload`.  
        
* ####Extended Server Configuration:

    The server can be configured in the _env_ section of either the _src/flashpolicy.app.src_ file or the _ebin/flashpolicy.app_ file. If you edit the _src/flashpolicy.app.src_ file you must run `./build.sh` again, that will copy it to the _ebin_ directory and __overwrite__ the _ebin/flashpolicy.app_ file. __Changing__ the server __configuration__ requires a __server restart__: `./policyserver stop && sudo ./policyserver start`.
   
          {env, [
            {policy_file, "./flashpolicy.xml"},         %% string(): policyfile to serve
        
            {enable_logging, true},                     %% boolean(): enable or disable logging
            {logfile_path, "./log/"},                   %% string(): path to logfiles. must end with path separator '/'
        
            {listen_at_interface, any},                 %% any | e.g. {192,168,0,2}: the ip address as tuple to bind at, or 'any' to listen at all interfaces
            {port, 843},                                %% integer(): the port to listen at
            
            {cert_file, "./ssl/host.cert"},             %% the name of the certificate file
            {key_file, "./ssl/host.key"},               %% the name of the secret key file for the certificate
            {key_pwd, none},                            %% the passphrase for the keyfile, if it is protected by a passphrase or none, if it is not protected
            {intermediate_cert_file, none},             %% the name of the file containing the intermediate certificate(s), or none if there aren't any
            
            {bind_also_at, []}                          %% [{interface(), port(), policy_file()}]: additional interfaces and ports to listen at, e.g [{any, 8080, "./otherPolicy.xml"}]
          ]}

  
    _bind_also_at_ can be used to serve different policy files at different ports or interfaces.
  
* ####Logging:

    Logging can be enabled or disabled during runtime using `./policyserver enable-logging` or `./policyserver disable-logging`. The default logging directory is _./log_.

