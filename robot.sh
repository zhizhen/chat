#/usr/bin env

erl -pa ebin deps/*/ebin -eval "application:start(emqttc),robot:start_link(123)."
