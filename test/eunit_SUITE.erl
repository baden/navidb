-module(eunit_SUITE).
-compile(export_all).

all() ->
	[eunit].

eunit(_) ->
	ok = eunit:test({application, navidb}).
