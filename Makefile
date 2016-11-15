PROJECT = chat
PROJECT_DESCRIPTION = MQTT chat server
PROJECT_VERSION = 0.1

DEPS = esockd emqttd
dep_esockd = git https://github.com/emqtt/esockd emq20
dep_emqttd = git https://github.com/emqtt/emqttd master

include erlang.mk
