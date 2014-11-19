.PHONY: deps

PROJECT=navidb
REBAR=./rebar

all: deps compile tests

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

compile_dev: deps
	$(REBAR) -C rebar_ct.config compile

# tests: deps
#	$(REBAR) ct skip_deps=true
tests: compile_dev
	$(REBAR) -C rebar_ct.config ct skip_deps=true

clean:
	$(REBAR) clean delete-deps

console:
	erl -pa ebin deps/*/ebin -config test/test.config

travis:
	$(REBAR) clean delete-deps
	$(REBAR) -C rebar_ct.config get-deps
	$(REBAR) -C rebar_ct.config compile
	# $(REBAR) compile -Dno_debug_info -DTEST
	# $(REBAR) ct skip_deps=true -v
	$(REBAR) -C rebar_ct.config ct skip_deps=true -v
