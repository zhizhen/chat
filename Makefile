REBAR=./rebar

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

gen:
	@$(REBAR) compile
	@$(REBAR) generate

