.PHONY: deps

PROJECT=navidb
REBAR=./rebar

all: deps compile tests

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

# tests: deps
#	$(REBAR) ct skip_deps=true

clean:
	$(REBAR) clean delete-deps

travis:
	$(REBAR) clean delete-deps
	$(REBAR) get-deps
	$(REBAR) -C rebar_ct.config compile
	$(REBAR) -C rebar_ct.config ct skip_deps=true -v
