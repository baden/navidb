-module(external).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

test(_) ->
    ct:pal("Hello").
