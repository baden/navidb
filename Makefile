# See LICENSE for licensing information.
.SILENT:

PROJECT = navidb

# Options.
# ERLC_OPTS = -Werror +debug_info

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars
ERLC_OPTS += +warn_unused_import +warn_unused_function +warn_bif_clash
ERLC_OPTS += +warn_unused_record +warn_deprecated_function +warn_obsolete_guard
ERLC_OPTS += +strict_validation +warn_export_vars +warn_exported_vars
ERLC_OPTS += +warn_missing_spec +warn_untyped_record +debug_info

# COMPILE_FIRST = cowboy_middleware cowboy_sub_protocol

# Dependencies error in erlang.mk?
# CT_OPTS += -pa test -erl_args -config test/test.config -pa deps/mongodb/deps/*/ebin
# TEST_ERLC_OPTS = +'{parse_transform, eunit_autoexport}'
CT_OPTS += -spec test.spec -cover test/cover.spec -erl_args -config test/test.config

PLT_APPS = crypto public_key

# Dependencies.


DEPS = mongodb
# dep_mongodb = git git://github.com/baden/mongodb-erlang.git refresh
# 8f751af or 8f751af18b9ce1c12f36355cb7a961efdfc718ee
dep_mongodb = git https://github.com/comtihon/mongodb-erlang v3.0.2

TEST_DEPS = ct_helper xref_runner
dep_ct_helper = git https://github.com/ninenines/ct_helper master

CI_OTP = OTP-17.5.6.6 OTP-18.2.1 OTP-18.1

BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk
# ESCRIPT_NAME = fprof_totals_cli
# ESCRIPT_FILE = fproftotals

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

EDOC_DIRS := ["src"]
EDOC_OPTS := {preprocess, true}, {source_path, ${EDOC_DIRS}}, nopackages, {subpackages, true}

include erlang.mk

# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

# dialyze-filtered:
# 	dialyzer --no_native `$(call erlang,$(call filter_opts.erl,$(ERLC_OPTS)))` $(DIALYZER_DIRS) $(DIALYZER_OPTS) \
# 	| fgrep --invert-match --file .dialyzer.ignore

test-shell: app
	erl -pa ebin -pa deps/*/ebin -pa test -config test/test.config
	# erl -pa ebin -pa deps/*/ebin -pa test -s sync -s navipoint -config test/test.config

typer::
	typer $(DIALYZER_PLT) -I deps -r src
