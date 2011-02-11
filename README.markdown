
Erlang Flash Policy File Server
==============================

###Erlang Server serving flash crossdomain policies on adobe standard port 843, intended for use in production environments.

* Building Server:

        erlc -I include -o ebin -v src/*.erl


* Testing Server:

        sudo erl -pa ./ebin -flashpolicy -run flashpolicy_app start

        perl -e 'printf "<policy-file-request/>%c",0' | nc 127.0.0.1 843

* Starting __Production__ Server:

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy -run flashpolicy_app start

* Starting Production Server binding __only at given interface__ _192.168.12.13_ at __custom port__ _1234_:

        erl -pa ./ebin -noshell -noinput -detached -flashpolicy listen_at_interface '{192,168,12,13}' port '1234' -run flashpolicy_app start

* Starting Production Server binding at all interfaces at default port _843_ __and additional port__ _1234_:

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy bind_also_at '[{any, 1234, "./flashpolicy.xml"}]' -run flashpolicy_app start

* Starting Production Server and __disable logging__:

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy enable_logging false -run flashpolicy_app start

* Starting Production Server and __log to custom location__ (must end with path separator):

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy logfile_path '"/var/log/flashpolicy/"' -run flashpolicy_app start
