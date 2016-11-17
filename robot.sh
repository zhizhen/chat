#/usr/bin env

erl -pa ebin deps/*/ebin -eval "application:start(emqttc),robot_manager:start_link(),robot_manager:login_many(1)."
