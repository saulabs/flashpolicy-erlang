
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


