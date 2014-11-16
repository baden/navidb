-module(gps_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [test1].

init_per_suite(Config) ->
    error_logger:tty(false),
    {ok, Modules} = application:ensure_all_started(navidb),
    % meck:new(navidb_mongodb, [non_strict]),
    [{modules, Modules} | Config].

end_per_suite(Config) ->
    % meck:unload(navidb_mongodb),
    Modules = ?config(modules, Config),
    [application:stop(Module) || Module <- lists:reverse(Modules)],
    application:unload(navidb),
    meck:unload(),
    error_logger:tty(true),
    ok.

test1(_) ->
    #{'_id' := Skey} = _System = helper:fake_system(),

    Hour = 10,
    Data = <<"fake-data">>,
    Res1 = navidb_gpsdb:save(Skey, Hour, Data),
    ct:pal("Res1 = ~p", [Res1]),
    navidb_gpsdb:flush(Skey),
    Hours = navidb:get_gps_hours(Skey, 0, 20),
    ct:pal("Hours = ~p", [Hours]),

    navidb:remove(systems, #{'_id' => Skey}),
    ok.
