
Erlang Flash Policy File Server
==============================

###Erlang Server serving flash crossdomain policies on adobe standard port 843, intended for use in production environments.

* Building Server:

        erlc -I include -o ebin -v src/*.erl


* Testing Server:

        sudo erl -pa ./ebin -flashpolicy -run flashpolicy_app start

        perl -e 'printf "<policy-file-request/>%c",0' | nc 127.0.0.1 843

* Starting Production Server:

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy -run flashpolicy_app start

* Starting Production Server binding _only at given interface_ 192.168.12.13 at _custom port_ 1234

        erl -pa ./ebin -noshell -noinput -detached -flashpolicy listen_at_interface '{192,168,12,13}' port '1234' -run flashpolicy_app start

* Starting Production Server binding at all interfaces at default port 843 _and additional port 1234_:

        sudo erl -pa ./ebin -noshell -noinput -detached -flashpolicy bind_also_at '[{any, 1234, "./flashpolicy.xml"}]' -run flashpolicy_app start

