PROJECT = chat
PROJECT_DESCRIPTION = MQTT chat server
PROJECT_VERSION = 0.1

DEPS = esockd emqttd emqttc mysql poolboy
dep_esockd = git https://github.com/emqtt/esockd emq20
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_emqttc = git https://github.com/emqtt/emqttc master
dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.2.0
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.1

LOCAL_DEPS = crypto sasl mnesia

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	cuttlefish -l info -e rel/ -c etc/emq_chat.conf -i priv/emq_chat.schema -d data
