-module(gps_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-import(ct_helper, [doc/1]).

all() -> [test1, test2, remove].

init_per_suite(Config) ->
    error_logger:tty(false),
    % Res = application:start(navidb),
    {ok, _Modules} = application:ensure_all_started(navidb),
    OTPRel = erlang:system_info(otp_release),
    % [{modules, Modules}, {otp_release, OTPRel} | Config].
    [{otp_release, OTPRel} | Config].

end_per_suite(_Config) ->
    % Modules = ?config(modules, Config),
    % [application:stop(Module) || Module <- lists:reverse(Modules)],
    % application:unload(navidb),
    % application:stop(navidb),
    error_logger:tty(true),
    ok.

init_per_testcase(_Case, Config) ->
    #{id := Skey} = _System = helper:fake_system(),
	[{skey, Skey} | Config].

end_per_testcase(_Case, Config) ->
    Skey = ?config(skey, Config),
    navidb:remove(systems, #{id => Skey}),
	ok.

test1(Config) ->
    doc("Test save functionality."),
    Skey = ?config(skey, Config),
    ok = navidb_gpsdb:save(Skey, 10, <<"fake-data1">>),
    ok = navidb_gpsdb:save(Skey, 10, <<"fake-data2">>),
    ok = navidb_gpsdb:save(Skey, 11, <<"fake-data3">>),
    ok = navidb_gpsdb:save(Skey, 11, <<"fake-data4">>),
    ?assertMatch([], lists:sort(navidb:get_gps_hours(Skey, 20, 30))),
    ?assertMatch([10, 11], lists:sort(navidb:get_gps_hours(Skey, 0, 20))),
    {ok, Geos} = navidb:get_geos(Skey, 0, 20),
    ?assertMatch(<<"fake-data1", "fake-data2", "fake-data3", "fake-data4">>, Geos),
    % Проверим что записи последнего часа (11) находятся в кеше.
    Status = navidb_gpsdb:info(),
    Info = maps:get(Skey, Status),
    ?assertMatch(#{hour := 11, data_length := 20}, Info),
    % Сбросим кеш
    navidb_gpsdb:flush(Skey),
    Status1 = navidb_gpsdb:info(),
    case ?config(otp_release, Config) of
        "17" -> ?assertException(error, bad_key, maps:get(Skey, Status1));
        "18" -> ?assertException(error, {badkey, _}, maps:get(Skey, Status1));
        "19" -> ?assertException(error, {badkey, _}, maps:get(Skey, Status1));
        "20" -> ?assertException(error, {badkey, _}, maps:get(Skey, Status1))
    end,

    % ?assertException(error, badkey(?config(otp_release, Config), _), maps:get(Skey, Status1)),
    % ?assertMatch(#{hour := 11, data_length := 20}, Info),
    ok.

% badkey("17", _Key) -> {error, bad_key};
% badkey("18", Key) -> {error, {badkey, Key}}.

test2(_Config) ->

    #{id := Skey1} = helper:fake_system(),
    #{id := Skey2} = helper:fake_system(),
    #{id := Skey3} = helper:fake_system(),

    ok = navidb_gpsdb:save(Skey1, 10, <<"fake-data1">>),
    ok = navidb_gpsdb:save(Skey2, 10, <<"fake-data2">>),
    ok = navidb_gpsdb:save(Skey3, 10, <<"fake-data3">>),

    {ok, 10, <<"fake-data1">>} = navidb_gpsdb:get(Skey1),
    {ok, 10, <<"fake-data2">>} = navidb_gpsdb:get(Skey2),
    {ok, 10, <<"fake-data3">>} = navidb_gpsdb:get(Skey3),

    navidb_gpsdb:flush(all),

    nodata = navidb_gpsdb:get(Skey1),
    nodata = navidb_gpsdb:get(Skey2),
    nodata = navidb_gpsdb:get(Skey3),

    navidb:remove(systems, #{id => Skey1}),
    navidb:remove(systems, #{id => Skey2}),
    navidb:remove(systems, #{id => Skey3}),
    ok.

remove(Config) ->
    Skey = ?config(skey, Config),

    Hour = 10,
    Data = <<"fake-data">>,
    ok = navidb_gpsdb:save(Skey, Hour, Data),
    [10] = navidb:get_gps_hours(Skey, 0, 20),

    Selector = #{
        <<"system">> => Skey,
        <<"hour">> => #{
            <<"$gte">> => 9,
            <<"$lte">> => 11
        }
    },
    navidb:remove(gps, Selector, {flush, {gps, Skey}}),

    [] = navidb:get_gps_hours(Skey, 0, 20),

    ok.
