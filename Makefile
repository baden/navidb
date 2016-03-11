# See LICENSE for licensing information.

PROJECT = navidb

# Options.

# ERLC_OPTS = -Werror +debug_info

ERLC_OPTS = -Werror +debug_info +warn_export_all +warn_export_vars \
	+warn_shadow_vars +warn_obsolete_guard
# +warn_missing_spec
# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol

# Dependencies error in erlang.mk?
# CT_OPTS += -pa test -erl_args -config test/test.config -pa deps/mongodb/deps/*/ebin
# TEST_ERLC_OPTS = +'{parse_transform, eunit_autoexport}'
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config
PLT_APPS = crypto public_key

# Dependencies.

DEPS = mongodb
dep_mongodb = git git://github.com/baden/mongodb-erlang.git refresh

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master


CI_OTP = OTP-17.5.6.6 OTP-18.2.1 OTP-18.1


# TEST_DEPS = ct_helper meck
# TEST_DEPS = ct_helper
# dep_ct_helper = git https://github.com/extend/ct_helper.git master
# dep_meck = git git://github.com/eproxus/meck.git 0.8.2

# {lager,   ".*", {git, "git://github.com/basho/lager.git",        {branch, "master"}}},
# {cowboy,  ".*", {git, "git://github.com/extend/cowboy.git",      {tag, "2.0.0-pre.1"}}},
# {jsxn,    ".*", {git, "git://github.com/talentdeficit/jsxn.git", {tag, "v2.1.1"}}},
# {folsom,  ".*", {git, "https://github.com/boundary/folsom.git",  {branch, "master"}}},
# {navidb,  ".*", {git, "git://github.com/baden/navidb.git",       {branch, "master"}}}

# Standard targets.

include erlang.mk

# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -s navipoint -config test/test.config
	# erl -pa ebin -pa deps/*/ebin -pa test -s sync -s navipoint -config test/test.config

.PHONY: update-erlang.mk

update-erlang.mk:
	#wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
	curl -O https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk

.PHONY: test-in-docker

test-in-docker:
	docker-compose up
