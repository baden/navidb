-module(gps_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [test1, remove].

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

init_per_testcase(_Case, Config) ->
    #{'_id' := Skey} = _System = helper:fake_system(),
	[{skey, Skey} | Config].

end_per_testcase(_Case, Config) ->
    Skey = ?config(skey, Config),
    navidb:remove(systems, #{'_id' => Skey}),
	ok.

test1(Config) ->
    Skey = ?config(skey, Config),
    ok = navidb_gpsdb:save(Skey, 10, <<"fake-data1">>),
    ok = navidb_gpsdb:save(Skey, 10, <<"fake-data2">>),
    ok = navidb_gpsdb:save(Skey, 11, <<"fake-data3">>),
    ok = navidb_gpsdb:save(Skey, 11, <<"fake-data4">>),
    ?assertMatch([10, 11], lists:sort(navidb:get_gps_hours(Skey, 0, 20))),
    {ok, Geos} = navidb:get_geos(Skey, 0, 20),
    ct:pal("Geos = ~p", [Geos]),
    ?assertMatch(<<"fake-data1", "fake-data2", "fake-data3", "fake-data4">>, Geos),
    % Проверим что записи последнего часа (11) находятся в кеше.
    Status = navidb_gpsdb:info(),
    ct:pal("Status = ~p", [Status]),
    Info = maps:get(Skey, Status),
    ct:pal("  Info = ~p", [Info]),
    ?assertMatch(#{hour := 11, data_length := 20}, Info),
    % Сбросим кеш
    navidb_gpsdb:flush(Skey),
    Status1 = navidb_gpsdb:info(),
    ct:pal("Status1 = ~p", [Status1]),
    ?assertException(error, bad_key, maps:get(Skey, Status1)),
    % ct:pal("  Info1 = ~p", [Info1]),
    % ?assertMatch(#{hour := 11, data_length := 20}, Info),
    ok.

remove(Config) ->
    Skey = ?config(skey, Config),

    Hour = 10,
    Data = <<"fake-data">>,
    ok = navidb_gpsdb:save(Skey, Hour, Data),
    [10] = navidb:get_gps_hours(Skey, 0, 20),

    Selector = #{
        'system' => Skey,
        'hour' => #{
            '$gte' => 9,
            '$lte' => 11
        }
    },
    navidb:remove(gps, Selector, {flush, {gps, Skey}}),

    [] = navidb:get_gps_hours(Skey, 0, 20),

    ok.
